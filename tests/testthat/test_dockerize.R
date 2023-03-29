.generate_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

test_that("defensive programming", {
    graph <- readRDS("../testdata/sle_graph.RDS")
    expect_error(dockerize(graph, output_dir = tempdir()))
})

test_that("empty rang dockerize #75", {
    graph <- readRDS("../testdata/rang_ok.RDS")
    graph$ranglets <- list()
    expect_warning(x <- dockerize(graph, output_dir = .generate_temp_dir()))
    expect_equal(x, NULL)
})

test_that("integration of #13 in dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## rang_as_comment = TRUE
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^## ## To reconstruct this file", x)))
    expect_false(any(grepl("^## ## WARNING", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, rang_as_comment = FALSE)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_false(any(grepl("^## ## To reconstruct this file", x)))
})

test_that("integration of #16 in dockerize()", {
    ## verbose
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## verbose = TRUE
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^verbose <- TRUE", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, verbose = FALSE)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^verbose <- FALSE", x)))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN Rscript rang\\.R", Dockerfile)))
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
    expect_false(any(grepl("^RUN Rscript rang\\.R", Dockerfile)))
    ## #123
    expect_equal(tail(Dockerfile, 1), "CMD [\"R\"]")
})

test_that("integration of #18 in dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "cran.r-project.org")
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(dockerize(rang = rang_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})

test_that("integration of #20 to dockerize()", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(rang_ok$r_version, "4.2.2")
    temp_dir <- .generate_temp_dir()
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.3.0"
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.2.0"
    dockerize(rang_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_false(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    expect_true(any(grepl("^cran.mirror <- \"http://cran\\.r\\-project\\.org/\"", x)))
})

test_that("Dockerize R < 3.1 and >= 2.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    expect_equal(rang_rio$r_version, "3.0.1")
    temp_dir <- .generate_temp_dir()
    dockerize(rang_rio, output_dir = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "compile_r.sh")))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN bash compile_r.sh 3.0.1", Dockerfile)))
    expect_equal(tail(Dockerfile, 1), "CMD [\"R\"]")
    ## lib
    dockerize(rang_rio, output_dir = temp_dir, lib = "abc")
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN mkdir", Dockerfile)))
    expect_equal(tail(Dockerfile, 1), "CMD [\"R\"]")
})

test_that("Docker R < 1.3.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "1.3.1" ## exactly 1.3.1, no error
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(rang_rio, output_dir = temp_dir)) ## no cache
    ##expect_error(dockerize(rang_rio, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "1.3.0"
    expect_error(dockerize(rang_rio, output_dir = temp_dir))
})

test_that(".group_sysreqs and issue #21", {
    graph <- readRDS("../testdata/graph.RDS")
    expect_equal(.group_sysreqs(graph), "apt-get update -qq && apt-get install -y libpcre3-dev zlib1g-dev pkg-config libcurl4-openssl-dev && apt-get install -y default-jdk libgsl0-dev libicu-dev libpng-dev libxml2-dev make python3 zlib1g-dev liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    graph <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(.group_sysreqs(graph), "apt-get update -qq && apt-get install -y libpcre3-dev zlib1g-dev pkg-config libcurl4-openssl-dev")
    graph <- readRDS("../testdata/issue21.RDS")
    expected_output <- "apt-get update -qq && apt-get install -y libpcre3-dev zlib1g-dev pkg-config && apt-get install -y software-properties-common && add-apt-repository -y ppa:cran/libgit2 && apt-get update && apt-get install -y cmake git libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libgit2-dev libgsl0-dev libharfbuzz-dev libicu-dev libjpeg-dev libpng-dev libssh2-1-dev libssl-dev libtiff-dev libxml2-dev make pandoc pari-gp zlib1g-dev"
    expect_warning(output <- .group_sysreqs(graph))
    expect_equal(output, expected_output)
    graph <- readRDS("../testdata/issue21_ubuntu2004.RDS")
    expect_warning(output <- .group_sysreqs(graph), NA)
    expect_true(grepl("gnutls", output))
})

test_that("Dockerize warning, issue #21", {
    graph <- readRDS("../testdata/issue21.RDS")
    temp_dir <- .generate_temp_dir()
    expect_warning(dockerize(graph, output_dir = temp_dir))
})

test_that("material_dir, non-existing, #23", {
    ## normal case
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(rang_rio, output_dir = temp_dir, materials_dir = NULL), NA)
    ## non-existing
    fake_material_dir <- .generate_temp_dir()
    expect_false(dir.exists(fake_material_dir))
    expect_error(dockerize(rang_rio, output_dir = temp_dir, materials_dir = fake_material_dir))
})

test_that("material_dir, existing, no subdir, #23", {
    ## exist, but empty dir
    ## Pre R 3.1.0
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    temp_dir <- .generate_temp_dir()
    fake_material_dir <- .generate_temp_dir()
    dir.create(fake_material_dir)
    dockerize(rang_rio, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_equal(list.files(file.path(temp_dir, "materials")), character(0))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "COPY materials/ ./materials/"))
    ## Post R 3.1.0
    graph <- readRDS("../testdata/graph.RDS")
    temp_dir <- .generate_temp_dir()
    fake_material_dir <- .generate_temp_dir()
    dir.create(fake_material_dir)
    dockerize(graph, output_dir = temp_dir, materials_dir = fake_material_dir)
    expect_true(dir.exists(file.path(temp_dir, "materials")))
    expect_equal(list.files(file.path(temp_dir, "materials")), character(0))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(Dockerfile == "COPY materials/ ./materials/"))
    expect_equal(tail(Dockerfile, 1), "CMD [\"R\"]")
    ## Will only test post 3.1.0 from now on
    ## some files in fake_material_dir
    temp_dir <- .generate_temp_dir()
    fake_material_dir <- .generate_temp_dir()
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
    temp_dir <- .generate_temp_dir()
    fake_material_dir <- .generate_temp_dir()
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

test_that("readme issue #50", {
    skip_on_os("windows", arch = NULL)
    skip_on_cran()
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(rang_ok$r_version, "4.2.2")
    temp_dir <- .generate_temp_dir()
    dockerize(rang_ok, output_dir = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "README")))
    content <- readLines(file.path(temp_dir, "README"))
    expect_true(any(grepl(temp_dir, content)))
})

test_that("#123 rstudio", {
    graph <- readRDS("../testdata/graph.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(graph, output_dir = temp_dir, image = "rstudio")
    dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true("EXPOSE 8787" %in% dockerfile)
    expect_equal(tail(dockerfile, 1), "CMD [\"/init\"]")
})

test_that("dockerize with bioc #58", {
  rang_bioc <- readRDS("../testdata/rang_bioc.RDS")
  temp_dir <- .generate_temp_dir()
  dockerize(rang = rang_bioc, output_dir = temp_dir) ## verbose = TRUE
  x <- readLines(file.path(temp_dir, "rang.R"))
  expect_true(any(grepl("bioc.mirror",x)))
})

test_that("no_rocker #67", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir) ## no_rocker = FALSE
    expect_false(file.exists(file.path(temp_dir, "compile_r.sh")))
    expect_false(any(readLines(file.path(temp_dir, "Dockerfile")) == "FROM debian/eol:lenny"))
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir, no_rocker = TRUE) ## debian_version = lenny
    expect_true(file.exists(file.path(temp_dir, "compile_r.sh")))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "FROM debian/eol:lenny"))
    temp_dir <- .generate_temp_dir()
    dockerize(rang = rang_ok, output_dir = temp_dir, no_rocker = TRUE,
              debian_version = "jessie")
    expect_true(file.exists(file.path(temp_dir, "compile_r.sh")))
    expect_true(any(readLines(file.path(temp_dir, "Dockerfile")) == "FROM debian/eol:jessie"))
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(rang = rang_ok, output_dir = temp_dir, no_rocker = TRUE,
              debian_version = "3.11"))
})

test_that(".check_tarball_path", {
    expect_error(.check_tarball_path("../testdata/gesis_2.0.tar.gz", "gesis")) ##dir = FALSE
    expect_error(.check_tarball_path("../testdata/askpass_1.1.tar.gz", "askpass"), NA)
    expect_error(.check_tarball_path("../testdata/gesis", "gesis", dir = TRUE))
    expect_error(.check_tarball_path("../testdata/askpass", "askpass", dir = TRUE), NA)
})
