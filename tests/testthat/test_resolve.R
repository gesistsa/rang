test_that("defensive programming", {
    expect_error(resolve("LDAvis", os = "windows"))
})
