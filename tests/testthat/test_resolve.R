.gen_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

test_that("defensive programming", {
    expect_error(resolve("LDAvis", os = "windows"))
})

## The following are real tests. Even with memoisation, please keep at minimum

test_that("normal", {
    ## normal case
    skip_if_offline()
    skip_on_cran()
    expect_silent(x <- resolve("LDAvis", snapshot_date = "2023-01-01"))
    expect_equal(x$unresolved_pkgrefs, character(0))
    expect_equal(x$deps_sysreqs, character(0))
    expect_equal(x$r_version, "4.2.2")
})

test_that("unresolved", {
    ## too early for LDAvis
    skip_if_offline()
    skip_on_cran()
    expect_error(res <- .get_snapshot_dependencies("cran::LDAvis", snapshot_date = "2001-10-01"))
    warns <- capture_warnings(x <- resolve("LDAvis", snapshot_date = "2000-10-01"))
    expect_equal(x$r_version, "1.1.1")    
    expect_true(length(warns) == 2)
    expect_true(any(grepl("^Some package", warns)))
    expect_true(any(grepl("^No packages to query", warns)))
    expect_equal(x$unresolved_pkgrefs, "cran::LDAvis")
    expect_equal(x$ranglets, list())
    expect_true(is.na(x$deps_sysreqs))
    ## a mixture; sna is old enough
    warns2 <- capture_warnings(x2 <- resolve(c("sna", "LDAvis"), snapshot_date = "2001-10-01"))
    expect_true(length(warns2) == 1)
    expect_true(any(grepl("^Some package", warns2)))
    expect_false(any(grepl("^No packages to query", warns2)))
    expect_equal(x2$unresolved_pkgrefs, "cran::LDAvis")
    expect_false(length(x2$ranglets) == 0)
    expect_equal(x2$deps_sysreqs, character(0))
})

test_that("issue #19", {
    skip_if_offline()
    skip_on_cran()
    warns1 <- capture_warnings(x <- resolve("gRbase", snapshot_date = "2005-12-01"))
    expect_true(length(warns1) == 1)
    expect_true(any(grepl("^cran::dynamicGraph can't", warns1)))
    expect_true(length(x$deps_sysreqs) != 0)
})

test_that("cache #17", {
    skip_if_offline()
    skip_on_cran()
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .gen_temp_dir()
    dockerize(rang_ok, output_dir = temp_dir) ## cache = FALSE
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^COPY cache", x)))
    expect_false(dir.exists(file.path(temp_dir, "cache")))
    expect_false(file.exists(file.path(temp_dir, "cache", "LDAvis_0.3.2.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache", "proxy_0.4-27.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache", "RJSONIO_1.3-1.6.tar.gz")))
    expect_silent(dockerize(rang_ok, output_dir = temp_dir, cache = TRUE, verbose = FALSE))
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache")))
    expect_true(file.exists(file.path(temp_dir, "cache", "LDAvis_0.3.2.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "proxy_0.4-27.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "RJSONIO_1.3-1.6.tar.gz")))
    ## expect_output(dockerize(rang_ok, output_dir = temp_dir, cache = TRUE, verbose = TRUE))
})

test_that("cache for R < 3.1 and R >= 2.1", {
    skip_if_offline()
    skip_on_cran()
    rang_rio <- readRDS("../testdata/rang_rio_old.RDS")
    expect_equal(rang_rio$r_version, "3.0.1")
    temp_dir <- .gen_temp_dir()
    dockerize(rang_rio, output_dir = temp_dir, cache = TRUE, verbose = FALSE)
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache")))
    expect_true(file.exists(file.path(temp_dir, "cache", "rio_0.1.1.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "stringr_0.6.2.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "digest_0.6.3.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "foreign_0.8-54.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "evaluate_0.4.7.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache", "testthat_0.7.1.tar.gz")))
})

test_that(".system_requirements_github", {
    skip_if_offline()
    skip_on_cran()
    res <- .system_requirements_github("schochastics/rtoot", os = "ubuntu-20.04")
    expect_true(all(grepl("^apt-get", res)))
    res <- .system_requirements_github("schochastics/rtoot", "opensuse-42.3")
    expect_true(all(grepl("^zypper", res)))
    res <- .system_requirements_github("schochastics/rtoot", "centos-8")
    expect_true(all(grepl("^dnf", res)))
})

test_that("github correct querying; also #25", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve(c("cran/sna"), snapshot_date = "2020-05-01", get_sysreqs = FALSE)
    expect_equal(length(x$ranglets[[1]]$deps), 19)
    x <- resolve("schochastics/netUtils", snapshot_date = "2020-06-01", get_sysreqs = FALSE)
    expect_equal(x$ranglets[[1]]$pkgref, "github::schochastics/netUtils")
    expect_equal(unique(x$ranglets[[1]]$original$x), "igraphUtils")
    x <- resolve("tidyverse/stringr", snapshot_date = "2022-12-31")
    expect_true(all(grepl("^apt-get", x$deps_sysreqs)))
    x2 <- resolve("tidyverse/stringr", snapshot_date = "2022-12-31", get_sysreqs = FALSE)
    expect_equal(x2$deps_sysreqs, character(0))
    x2 <- query_sysreqs(x2)
    expect_equal(x2$deps_sysreqs, x$deps_sysreqs)
})

test_that("Non-cran must enforce caching ref #22", {
    skip_if_offline()
    skip_on_cran()
    temp_dir <- .gen_temp_dir()
    graph <- readRDS("../testdata/ancientsna.RDS")
    expect_equal(graph$ranglets[[1]]$pkgref, "github::cran/sna")
    expect_error(dockerize(graph, output_dir = temp_dir, verbose = FALSE)) ## cache = FALSE
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    temp_dir <- .gen_temp_dir()
    graph <- readRDS("../testdata/anciente1071.RDS")
    expect_equal(graph$ranglets[[1]]$pkgref, "cran::e1071")
    expect_error(dockerize(graph, output_dir = temp_dir, verbose = FALSE), NA)
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
})

## This should be tested. But this takes too long without cache.
## test_that("issue #21", {
##     skip_if_offline()
##     skip_on_cran()
##     expect_warning(x <- resolve("devtools", os = "ubuntu-18.04"))
##     expect_warning(x <- resolve("devtools", os = "ubuntu-20.04"), NA)
## })
