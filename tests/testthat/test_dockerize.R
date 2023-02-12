.gen_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

test_that("defensive programming", {
    graph <- readRDS("../testdata/sle_graph.RDS")
    expect_error(dockerize(graph, output_dir = tempdir()))
})

test_that("integration of #13 in dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## rang_as_comment = TRUE
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^## ## To reconstruct this file", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, rang_as_comment = FALSE)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_false(any(grepl("^## ## To reconstruct this file", x)))
})

test_that("integration of #16 in dockerize()", {
    ## verbose
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## verbose = TRUE
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^verbose <- TRUE", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, verbose = FALSE)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^verbose <- FALSE", x)))
    ## lib
    dockerize(rang = rang_ok, output_dir = temp_dir) ## lib = NA
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^lib <- NA", x)))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^RUN mkdir", Dockerfile)))
    dockerize(rang = rang_ok, output_dir = temp_dir, lib = "abc") ## lib = NA
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^lib <- \"abc\"", x)))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN mkdir", Dockerfile)))
})

test_that("integration of #18 in dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "cran.r-project.org")
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})

test_that("integration of #20 to dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(rang_ok$r_version, "4.2.2")
    temp_dir <- .gen_temp_dir()
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.3.0"
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.2.0"
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_false(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    expect_true(any(grepl("^cran_mirror <- \"http://cran\\.r\\-project\\.org/\"", x)))
})

test_that("Dockerize R < 3.1 and >= 2.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    expect_equal(rang_rio$r_version, "3.0.1")
    temp_dir <- .gen_temp_dir()
    dockerize(rang_rio, output_dir = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "compile_r.sh")))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN bash compile_r.sh 3.0.1", Dockerfile)))
    ## lib
    dockerize(rang_rio, output_dir = temp_dir, lib = "abc")
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN mkdir", Dockerfile)))
})

test_that("Docker R < 2.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "2.1.0" ## exactly 2.1.0, no error
    temp_dir <- .gen_temp_dir()
    expect_error(dockerize(rang_rio, output_dir = temp_dir), NA)
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "2.0.0"
    expect_error(dockerize(rang_rio, output_dir = temp_dir))
})

test_that(".consolidate_sysreqs and issue #21", {
    graph <- readRDS("../testdata/graph.RDS")
    expect_equal(.consolidate_sysreqs(graph), "apt-get update -qq && apt-get install -y default-jdk libgsl0-dev libicu-dev libpng-dev libxml2-dev make python3 zlib1g-dev liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    graph <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(.consolidate_sysreqs(graph), "apt-get update -qq")
    graph <- readRDS("../testdata/issue21.RDS")
    expected_output <- "apt-get update -qq && apt-get install -y software-properties-common && add-apt-repository -y ppa:cran/libgit2 && apt-get update && apt-get install -y cmake git libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libgit2-dev libgsl0-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev libssh2-1-dev libssl-dev libtiff-dev libxml2-dev make pandoc pari-gp zlib1g-dev"
    expect_warning(output <- .consolidate_sysreqs(graph))
    expect_equal(output, expected_output)
    graph <- readRDS("../testdata/issue21_ubuntu2004.RDS")
    expect_warning(output <- .consolidate_sysreqs(graph), NA)
    expect_true(grepl("gnutls", output))
})

test_that("Dockerize warning, issue #21", {
    graph <- readRDS("../testdata/issue21.RDS")
    temp_dir <- .gen_temp_dir()
    expect_warning(dockerize(graph, output_dir = temp_dir))
})

test_that("material_dir, non-existing, #23", {
    ## normal case
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    temp_dir <- .gen_temp_dir()
    expect_error(dockerize(rang_rio, output_dir = temp_dir, materials_dir = NULL), NA)
    ## non-existing
    fake_material_dir <- .gen_temp_dir()
    expect_false(dir.exists(fake_material_dir))
    expect_error(dockerize(rang_rio, output_dir = temp_dir, materials_dir = fake_material_dir))    
})

test_that("material_dir, existing, no subdir, #23", {
    ## exist, but empty dir
    ## Pre R 3.1.0
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    temp_dir <- .gen_temp_dir()
    fake_material_dir <- .gen_temp_dir()
    dir.create(fake_material_dir)
    dockerize(rang_rio, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_equal(list.files(file.path(temp_dir, "materials")), character(0))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "COPY materials/ ./materials/"))
    ## Post R 3.1.0
    graph <- readRDS("../testdata/graph.RDS")
    temp_dir <- .gen_temp_dir()
    fake_material_dir <- .gen_temp_dir()
    dir.create(fake_material_dir)
    dockerize(graph, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_equal(list.files(file.path(temp_dir, "materials")), character(0))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "COPY materials/ ./materials/"))
    ## Will only test post 3.1.0 from now on
    ## some files in fake_material_dir
    temp_dir <- .gen_temp_dir()
    fake_material_dir <- .gen_temp_dir()
    dir.create(fake_material_dir)
    file.copy("../testdata/graph.RDS", file.path(fake_material_dir, "graph.RDS"))
    writeLines(c("831721", "GESIS"), file.path(fake_material_dir, "test.R"))
    dockerize(graph, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_equal(list.files(file.path(temp_dir, "materials")), c("graph.RDS", "test.R"))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "COPY materials/ ./materials/"))
    expect_true(file.exists(file.path(temp_dir, "materials", "graph.RDS")))
    expect_true(file.exists(file.path(temp_dir, "materials", "test.R")))
    content <- readLines(file.path(temp_dir, "materials", "test.R"))
    expect_equal(content[1], "831721")
    expect_equal(content[2], "GESIS")
})

test_that("material_dir, existing, with 1 subdir, #23", {
    temp_dir <- .gen_temp_dir()
    fake_material_dir <- .gen_temp_dir()
    dir.create(fake_material_dir)
    dir.create(file.path(fake_material_dir, "data"))
    file.copy("../testdata/graph.RDS", file.path(fake_material_dir, "data", "graph.RDS"))
    writeLines(c("831721", "GESIS"), file.path(fake_material_dir, "test.R"))
    graph <- readRDS("../testdata/graph.RDS")
    dockerize(graph, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "COPY materials/ ./materials/"))
    expect_true(dir.exists(file.path(temp_dir, "materials", "data")))
    expect_true(file.exists(file.path(temp_dir, "materials", "data", "graph.RDS")))
    expect_true(file.exists(file.path(temp_dir, "materials", "test.R")))
    content <- readLines(file.path(temp_dir, "materials", "test.R"))
    expect_equal(content[1], "831721")
    expect_equal(content[2], "GESIS")
})
