test_that("defensive programming", {
    graph <- readRDS("../testdata/sle_graph.RDS")
    expect_error(dockerize(graph, output_dir = tempdir()))
})

test_that("integration of #13 in dockerize()", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_dir <- file.path(tempdir(), sample(1:10000, size = 1))
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
    temp_dir <- file.path(tempdir(), sample(1:10000, size = 1))
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
    temp_dir <- file.path(tempdir(), sample(1:10000, size = 1))
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
    temp_dir <- file.path(tempdir(), sample(1:10000, size = 1))
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
