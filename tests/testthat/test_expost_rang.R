test_that("rang_as_comment or #13", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## default rang_as_comment = TRUE
    x <- readLines(temp_r)
    expect_true(any(grepl("^## ## To reconstruct this file", x)))
    export_rang(rang_ok, path = temp_r, rang_as_comment = FALSE) ## default rang_as_comment = TRUE
    x <- readLines(temp_r)
    expect_false(any(grepl("^## ## To reconstruct this file", x)))
})

test_that("verbose #16", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r, rang_as_comment = FALSE) ## verbose = TRUE
    x <- readLines(temp_r)
    expect_true(any(grepl("^verbose <- TRUE", x)))
    export_rang(rang_ok, path = temp_r, rang_as_comment = FALSE, verbose = FALSE)
    x <- readLines(temp_r)
    expect_true(any(grepl("^verbose <- FALSE", x)))
})

test_that("lib #16", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r, rang_as_comment = FALSE) ## lib = NA
    x <- readLines(temp_r)
    expect_true(any(grepl("^lib <- NA", x)))
    export_rang(rang_ok, path = temp_r, rang_as_comment = FALSE, verbose = FALSE, lib = "abc")
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

test_that(".normalize_url https issue #20", {
    x <- .normalize_url("http://cran.r-project.org/") # https = TRUE
    expect_equal(x, "https://cran.r-project.org/")
    x <- .normalize_url("http://cran.r-project.org/", https = FALSE)
    expect_equal(x, "http://cran.r-project.org/")
    x <- .normalize_url("cran.r-project.org", https = FALSE)
    expect_equal(x, "http://cran.r-project.org/")
    x <- .normalize_url("cran.r-project.org//", https = FALSE)
    expect_equal(x, "http://cran.r-project.org/")
})

test_that(".query_mirror_validity", {
    expect_true(.query_mirror_validity("https://cran.r-project.org/"))
    expect_true(.query_mirror_validity("https://cloud.r-project.org/"))
    expect_false(.query_mirror_validity("https://www.chainsawriot.com/"))
})

test_that("integration of mirror selection to `export_rang` #18", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_rang(rang_ok, path = temp_r, cran_mirror = "cran.r-project.org")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_rang(rang_ok, path = temp_r, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(export_rang(rang_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(export_rang(rang_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})

test_that("integration of https to `export_rang` #20", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(rang_ok$r_version, "4.2.2")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.3.0"
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.2.0"
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_false(any(grepl("^cran_mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    expect_true(any(grepl("^cran_mirror <- \"http://cran\\.r\\-project\\.org/\"", x)))
})

test_that("Docker R < 2.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "2.1.0" ## exactly 2.1.0, no error
    temp_r <- tempfile(fileext = ".R")
    expect_error(export_rang(rang_rio, path = temp_r), NA)
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "2.0.0"
    expect_error(export_rang(rang_rio, path = temp_r))
})

test_that("issue #38", {
    issue38 <- readRDS("../testdata/issue38.RDS")
    expect_error(export_rang(issue38, tempfile(fileext = ".R")), NA)
})
