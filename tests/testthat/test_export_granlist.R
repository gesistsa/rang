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
