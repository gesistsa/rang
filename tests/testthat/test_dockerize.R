test_that("defensive programming", {
    graph <- readRDS("../testdata/sle_graph.RDS")
    expect_error(dockerize(graph, output_dir = tempdir()))
})
