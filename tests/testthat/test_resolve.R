test_that("defensive programming", {
    expect_error(resolve("LDAvis", os = "windows"))
})

## The following are real tests. Even with memoisation, please keep at minimum

test_that("normal", {
    ## normal case
    skip_if_offline()
    skip_on_cran()
    expect_silent(x <- resolve("LDAvis", snapshot_date = "2023-01-01"))
    expect_equal(x$unresolved_pkgs, character(0))
    expect_equal(x$deps_sysreqs, character(0))
})

test_that("unresolved", {
    ## too early for LDAvis
    skip_if_offline()
    skip_on_cran()
    expect_error(res <- .get_snapshot_dependencies("LDAvis", snapshot_date = "2001-10-01"))
    warns <- capture_warnings(x <- resolve("LDAvis", snapshot_date = "2000-10-01"))
    expect_true(length(warns) == 2)
    expect_true(any(grepl("^Some package", warns)))
    expect_true(any(grepl("^No packages to query", warns)))
    expect_equal(x$unresolved_pkgs, "LDAvis")
    expect_equal(x$grans, list())
    expect_true(is.na(x$deps_sysreqs))
    ## a mixture; sna is old enough
    warns2 <- capture_warnings(x2 <- resolve(c("sna", "LDAvis"), snapshot_date = "2001-10-01"))
    expect_true(length(warns2) == 1)
    expect_true(any(grepl("^Some package", warns2)))
    expect_false(any(grepl("^No packages to query", warns2)))
    expect_equal(x2$unresolved_pkgs, "LDAvis")
    expect_false(length(x2$grans) == 0)
    expect_equal(x2$deps_sysreqs, character(0))
})

test_that("issue #19", {
    skip_if_offline()
    skip_on_cran()
    warns1 <- capture_warnings(x <- resolve("gRbase", snapshot_date = "2005-12-01"))
    expect_true(length(warns1) == 1)
    expect_true(any(grepl("^dynamicGraph can't", warns1)))
    expect_true(length(x$deps_sysreqs) != 0)
})

test_that("cache #17", {
    skip_if_offline()
    skip_on_cran()
    gran_ok <- readRDS("../testdata/gran_ok.RDS")
    temp_dir <- file.path(tempdir(), sample(1:10000, size = 1))
    dockerize(gran_ok, output_dir = temp_dir) ## cache = FALSE
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^COPY cache", x)))
    expect_false(dir.exists(file.path(temp_dir, "cache")))
    expect_false(file.exists(file.path(temp_dir, "cache", "LDAvis_0.3.2.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache", "proxy_0.4-27.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache", "RJSONIO_1.3-1.6.tar.gz")))
    expect_silent(dockerize(gran_ok, output_dir = temp_dir, cache = TRUE, verbose = FALSE))
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache")))
    expect_true(file.exists(file.path(temp_dir, "cache", "LDAvis_0.3.2.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "proxy_0.4-27.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "RJSONIO_1.3-1.6.tar.gz")))
    ## expect_output(dockerize(gran_ok, output_dir = temp_dir, cache = TRUE, verbose = TRUE))
})
