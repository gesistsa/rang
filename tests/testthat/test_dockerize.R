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
