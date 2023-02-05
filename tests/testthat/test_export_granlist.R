test_that("granlist_as_comment or #13", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_granlist(gran_ok, path = temp_r) ## default granlist_as_comment = TRUE
    x <- readLines(temp_r)
    expect_true(any(grepl("^## ## To reconstruct this file", x)))
    export_granlist(gran_ok, path = temp_r, granlist_as_comment = FALSE) ## default granlist_as_comment = TRUE
    x <- readLines(temp_r)
    expect_false(any(grepl("^## ## To reconstruct this file", x)))
})

test_that("verbose #16", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_granlist(gran_ok, path = temp_r, granlist_as_comment = FALSE) ## verbose = TRUE
    x <- readLines(temp_r)
    expect_true(any(grepl("^verbose <- TRUE", x)))
    export_granlist(gran_ok, path = temp_r, granlist_as_comment = FALSE, verbose = FALSE)
    x <- readLines(temp_r)
    expect_true(any(grepl("^verbose <- FALSE", x)))
})

test_that("lib #16", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_granlist(gran_ok, path = temp_r, granlist_as_comment = FALSE) ## lib = NA
    x <- readLines(temp_r)
    expect_true(any(grepl("^lib <- NA", x)))
    export_granlist(gran_ok, path = temp_r, granlist_as_comment = FALSE, verbose = FALSE, lib = "abc")
    x <- readLines(temp_r)
    expect_true(any(grepl("^lib <- \"abc\"", x)))
})

test_that(".normalize_url", {
    x <- .normalize_url("http://cran.r-project.org/")
    expect_equal(x, "https://cran.r-project.org/")
    x <- .normalize_url("cran.r-project.org/")
    expect_equal(x, "https://cran.r-project.org/")
    x <- .normalize_url("cran.r-project.org")
    expect_equal(x, "https://cran.r-project.org/")
    x <- .normalize_url("cran.r-project.org//")
    expect_equal(x, "https://cran.r-project.org/")
})

test_that(".check_mirror", {
    expect_true(.check_mirror("https://cran.r-project.org/"))
    expect_true(.check_mirror("https://cloud.r-project.org/"))
    expect_false(.check_mirror("https://www.chainsawriot.com/"))
})

test_that("integration of mirror selection to `export_granlist` #18", {
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_granlist(gran_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_granlist(gran_ok, path = temp_r, cran_mirror = "cran.r-project.org")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_granlist(gran_ok, path = temp_r, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(export_granlist(gran_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(export_granlist(gran_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})
