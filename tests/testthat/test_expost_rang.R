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
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_rang(rang_ok, path = temp_r, cran_mirror = "cran.r-project.org")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    export_rang(rang_ok, path = temp_r, cran_mirror = "https://cloud.r-project.org/")
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran.mirror <- \"https://cloud\\.r\\-project\\.org/\"", x)))
    expect_error(export_rang(rang_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/"))
    expect_error(export_rang(rang_ok, path = temp_r, cran_mirror = "https://www.chainsawriot.com/", check_cran_mirror = FALSE), NA)
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran.mirror <- \"https://www\\.chainsawriot\\.com/\"", x)))
})

test_that("integration of https to `export_rang` #20", {
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    expect_equal(rang_ok$r_version, "4.2.2")
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.3.0"
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_true(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    rang_ok$r_version <- "3.2.0"
    temp_r <- tempfile(fileext = ".R")
    export_rang(rang_ok, path = temp_r) ## cran_mirror = "https://cran.r-project.org/"
    x <- readLines(temp_r)
    expect_false(any(grepl("^cran.mirror <- \"https://cran\\.r\\-project\\.org/\"", x)))
    expect_true(any(grepl("^cran.mirror <- \"http://cran\\.r\\-project\\.org/\"", x)))
})

test_that("Docker R < 1.3.1", {
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "1.3.1" ## exactly 1.3.1, no error
    temp_r <- tempfile(fileext = ".R")
    expect_error(export_rang(rang_rio, path = temp_r), NA)
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    rang_rio$r_version <- "1.0.0"
    expect_error(export_rang(rang_rio, path = temp_r))
})

test_that("issue #38", {
    issue38 <- readRDS("../testdata/issue38.RDS")
    expect_error(export_rang(issue38, tempfile(fileext = ".R")), NA)
})

test_that("Bioconductor <2.0",{
    expect_error(.bioc_package_history(bioc_version = "1.9"))
})

test_that("Bioconductor new release",{
    expect_equal(.query_biocver("2023-01-01")$version,"3.16")
})

test_that("empty rang export, #75", {
    graph <- readRDS("../testdata/rang_ok.RDS")
    graph$ranglets <- list()
    expect_warning(x <- export_rang(graph, path = tempfile()))
    expect_equal(x, NULL)
})

test_that("prevent infinite loop, #81", {
    graph <- readRDS("../testdata/rang_ok.RDS")
    graph$ranglets[[1]]$deps[[2]] <- NULL
    expect_error(generate_installation_order(graph), "cran::LDAvis")
    graph <- readRDS("../testdata/rang_ok.RDS")
    graph$ranglets[[1]]$original$y <- "S4Vectors"
    graph$ranglets[[1]]$original$y_pkgref <- "bioc::S4Vectors"
    expect_error(generate_installation_order(graph), "cran::LDAvis")
})

test_that("renv export cran", {
  temp_dir <- tempdir()
  graph <- readRDS("../testdata/rang_ok.RDS")
  export_renv(graph, path = temp_dir)
  x <- readLines(file.path(temp_dir,"renv.lock"))
  expect_true(any(grepl("LDAvis",x)))
  expect_true(any(grepl("proxy",x)))
  expect_true(any(grepl("RJSONIO",x)))
})

test_that("renv export bioc", {
  temp_dir <- tempdir()
  graph <- readRDS("../testdata/rang_bioc.RDS")
  export_renv(graph, path = temp_dir)
  x <- readLines(file.path(temp_dir,"renv.lock"))
  expect_true(any(grepl("Bioconductor",x)))
})

test_that("renv export local and GH", {
  temp_dir <- tempdir()
  graph <- readRDS("../testdata/rang_local_gh.RDS")
  export_renv(graph, path = temp_dir)
  x <- readLines(file.path(temp_dir,"renv.lock"))
  expect_true(any(grepl("local",x)))
  expect_true(any(grepl("GitHub",x)))
})

test_that("empty renv export", {
  temp_dir <- tempdir()
  graph <- readRDS("../testdata/rang_ok.RDS")
  graph$ranglets <- list()
  expect_warning(x <- export_rang(graph, path = temp_dir))
  expect_equal(x, NULL)
})

test_that("renv export unknown source", {
  temp_dir <- tempdir()
  graph <- readRDS("../testdata/rang_ok.RDS")
  graph$ranglets[[1]]$original$x_pkgref <- "errr::or"
  expect_error(export_rang(graph, temp_dir))
})

test_that("Super ancient special packages", {
    graph <- readRDS("../testdata/superancientsna.RDS")
    expect_error(generate_installation_order(graph), NA)
})

test_that("base as a dependency, issue 144", {
    graph <- readRDS("../testdata/dt.RDS")
    expect_error(generate_installation_order(graph), NA)
})
