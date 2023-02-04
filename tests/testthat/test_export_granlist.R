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
