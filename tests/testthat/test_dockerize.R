.gen_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

test_that("defensive programming", {
    graph <- readRDS("../testdata/sle_graph.RDS")
    expect_error(dockerize(graph, output_dir = tempdir()))
})

test_that("integration of #13 in dockerize()", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(granlist = gran_ok, output_dir = temp_dir) ## granlist_as_comment = TRUE
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^## ## To reconstruct this file", x)))
    dockerize(granlist = gran_ok, output_dir = temp_dir, granlist_as_comment = FALSE)
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_false(any(grepl("^## ## To reconstruct this file", x)))
})

test_that("integration of #16 in dockerize()", {
    ## verbose
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(granlist = gran_ok, output_dir = temp_dir) ## verbose = TRUE
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^verbose <- TRUE", x)))
    dockerize(granlist = gran_ok, output_dir = temp_dir, verbose = FALSE)
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^verbose <- FALSE", x)))
    ## lib
    dockerize(granlist = gran_ok, output_dir = temp_dir) ## lib = NA
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^lib <- NA", x)))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^RUN mkdir", Dockerfile)))
    dockerize(granlist = gran_ok, output_dir = temp_dir, lib = "abc") ## lib = NA
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^lib <- \"abc\"", x)))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN mkdir", Dockerfile)))
})

test_that("integration of #18 in dockerize()", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(granlist = gran_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(granlist = gran_ok, output_dir = temp_dir, cran_mirror = "cran.r-project.org")
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    dockerize(granlist = gran_ok, output_dir = temp_dir, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(dockerize(granlist = gran_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(dockerize(granlist = gran_ok, output_dir = temp_dir, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})

test_that("integration of #20 to dockerize()", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    expect_equal(gran_ok$r_version, "4.2.2")
    temp_dir <- .gen_temp_dir()
    dockerize(gran_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    gran_ok$r_version <- "3.3.0"
    dockerize(gran_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "gran.R"))
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    gran_ok$r_version <- "3.2.0"
    dockerize(gran_ok, output_dir = temp_dir) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(file.path(temp_dir, "gran.R"))    
    expect_false(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    expect_true(any(grepl("^cran_mirror <- \"http://cran\\.r\\-project\\.org/\"", x)))
})

test_that("Dockerize R < 3.1 and >= 2.1", {
    gran_rio <- readRDS("../testdata/gran_rio_old.RDS")
    expect_equal(gran_rio$r_version, "3.0.1")
    temp_dir <- .gen_temp_dir()
    dockerize(gran_rio, output_dir = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "compile_r.sh")))
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN bash compile_r.sh 3.0.1", Dockerfile)))
    ## lib
    dockerize(gran_rio, output_dir = temp_dir, lib = "abc")
    Dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN mkdir", Dockerfile)))
})

test_that("Docker R < 2.1", {
    gran_rio <- readRDS("../testdata/gran_rio_old.RDS")
    gran_rio$r_version <- "2.1.0" ## exactly 2.1.0, no error
    temp_dir <- .gen_temp_dir()
    expect_error(dockerize(gran_rio, output_dir = temp_dir), NA)
    gran_rio <- readRDS("../testdata/gran_rio_old.RDS")
    gran_rio$r_version <- "2.0.0"
    expect_error(dockerize(gran_rio, output_dir = temp_dir))    
})

test_that(".consolidate_sysreqs and issue #21", {
    graph <- readRDS("../testdata/graph.RDS")
    expect_equal(.consolidate_sysreqs(graph), "apt-get update -qq && apt-get install -y default-jdk libxml2-dev make zlib1g-dev libpng-dev libgsl0-dev libicu-dev python3 liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    graph <- readRDS("../testdata/gran_ok.RDS")
    expect_equal(.consolidate_sysreqs(graph), "apt-get update -qq")
    graph <- readRDS("../testdata/issue21.RDS")
    expected_output <- "apt-get update -qq && apt-get install -y software-properties-common && add-apt-repository -y ppa:cran/libgit2 && apt-get update && apt-get install -y pari-gp pandoc libfreetype6-dev libfribidi-dev libharfbuzz-dev make git libxml2-dev libfontconfig1-dev cmake zlib1g-dev libpng-dev libjpeg-dev libgit2-dev libssl-dev libicu-dev libcurl4-openssl-dev libtiff-dev libgsl0-dev libssh2-1-dev"
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

