test_that("convert ranglet to edgelist", {
  graph <- readRDS("../testdata/graph.RDS")
  el <- convert_edgelist(graph$ranglets[[1]])
  expect_s3_class(el,"data.frame")
  expect_equal(nrow(el),4L)
})

test_that("convert rang to edgelist", {
  graph <- readRDS("../testdata/graph.RDS")
  el <- convert_edgelist(graph)
  expect_s3_class(el,"data.frame")
  expect_equal(nrow(el),121L)
})

test_that("convert edgelist empty rang", {
  graph <- readRDS("../testdata/rang_ok.RDS")
  graph$ranglets <- list()
  el <- convert_edgelist(graph)
  expect_s3_class(el,"data.frame")
  expect_equal(nrow(el),0L)
  
})


test_that("convert edgelist error", {
  expect_error(convert_edgelist("abc"))
  expect_error(convert_edgelist(42))
  expect_error(convert_edgelist(cbind(1:5,6:10)))
})

