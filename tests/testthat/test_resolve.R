.generate_temp_dir <- function() {
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
    expect_equal(x$sysreqs, character(0))
    expect_equal(x$r_version, "4.2.2")
})

test_that("unresolved", {
    ## too early for LDAvis
    skip_if_offline()
    skip_on_cran()
    expect_error(res <- .query_snapshot_dependencies("cran::LDAvis", snapshot_date = "2001-10-01"))
    warns <- capture_warnings(x <- resolve("LDAvis", snapshot_date = "2000-10-01"))
    expect_equal(x$r_version, "1.1.1")
    expect_true(length(warns) == 2)
    expect_true(any(grepl("^Some package", warns)))
    expect_true(any(grepl("^No packages to query", warns)))
    expect_equal(x$unresolved_pkgrefs, "cran::LDAvis")
    expect_equal(x$ranglets, list())
    expect_true(is.na(x$sysreqs))
    ## a mixture; sna is old enough
    warns2 <- capture_warnings(x2 <- resolve(c("sna", "LDAvis"), snapshot_date = "2001-10-01"))
    expect_true(length(warns2) == 1)
    expect_true(any(grepl("^Some package", warns2)))
    expect_false(any(grepl("^No packages to query", warns2)))
    expect_equal(x2$unresolved_pkgrefs, "cran::LDAvis")
    expect_false(length(x2$ranglets) == 0)
    expect_equal(x2$sysreqs, character(0))
})

test_that("issue #19", {
    skip_if_offline()
    skip_on_cran()
    warns1 <- capture_warnings(x <- resolve("gRbase", snapshot_date = "2005-12-01"))
    expect_true(length(warns1) == 1)
    expect_true(any(grepl("^cran::dynamicGraph can't", warns1)))
    expect_true(length(x$sysreqs) != 0)
})

test_that("cache #17", {
    skip_if_offline()
    skip_on_cran()
    rang_ok <- readRDS("../testdata/rang_ok.RDS")
    temp_dir <- .generate_temp_dir()
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
    temp_dir <- .generate_temp_dir()
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

test_that(".query_sysreqs_github", {
    skip_if_offline()
    skip_on_cran()
    res <- .query_sysreqs_github("schochastics/rtoot", os = "ubuntu-20.04")
    expect_true(all(grepl("^apt-get", res)))
    expect_true(length(res) == 2) ## issue #45
    res <- .query_sysreqs_github("schochastics/rtoot", "opensuse-42.3")
    expect_true(all(grepl("^zypper", res)))
    res <- .query_sysreqs_github("schochastics/rtoot", "centos-8")
    expect_true(all(grepl("^dnf", res)))
})

test_that("github correct querying; also #25", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve(c("cran/sna"), snapshot_date = "2020-05-01", query_sysreqs = FALSE)
    expect_equal(length(x$ranglets[[1]]$deps), 19)
    x <- resolve("schochastics/netUtils", snapshot_date = "2020-06-01", query_sysreqs = FALSE)
    expect_equal(x$ranglets[[1]]$pkgref, "github::schochastics/netUtils")
    expect_equal(unique(x$ranglets[[1]]$original$x), "igraphUtils")
    x <- resolve("tidyverse/stringr", snapshot_date = "2022-12-31")
    expect_true(all(grepl("^apt-get", x$sysreqs)))
    x2 <- resolve("tidyverse/stringr", snapshot_date = "2022-12-31", query_sysreqs = FALSE)
    expect_equal(x2$sysreqs, character(0))
    x2 <- query_sysreqs(x2)
    expect_equal(x2$sysreqs, x$sysreqs)
})

test_that("Non-cran must enforce caching ref #22", {
    skip_if_offline()
    skip_on_cran()
    temp_dir <- .generate_temp_dir()
    graph <- readRDS("../testdata/ancientsna.RDS")
    expect_equal(graph$ranglets[[1]]$pkgref, "github::cran/sna")
    expect_error(dockerize(graph, output_dir = temp_dir, verbose = FALSE)) ## cache = FALSE
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    temp_dir <- .generate_temp_dir()
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

test_that("Integration of as_pkgrefs() in resolve() for sessionInfo()", {
    x <- resolve(c("cran::sna"), snapshot_date = "2020-05-01", query_sysreqs = FALSE)
    si <- readRDS("../testdata/sessionInfo2.RDS")
    expect_error(graph <- resolve(si, snapshot_date = "2020-05-01", query_sysreqs = FALSE), NA)
    expect_equal(graph$ranglets[["cran::sna"]], x$ranglets[["cran::sna"]])
})

test_that("resolving packages from bioconductor", {
    skip_if_offline()
    skip_on_cran()
    expect_silent(x <- resolve("BiocGenerics", snapshot_date = "2022-10-20"))
    expect_equal(x$unresolved_pkgrefs, character(0))
    expect_equal(x$sysreqs, character(0))
    expect_equal(x$r_version, "4.2.1")
})

test_that("cache bioc pkgs", {
    skip_if_offline()
    skip_on_cran()
    rang_bioc <- readRDS("../testdata/rang_bioc.RDS")
    temp_dir <- .generate_temp_dir()
    dockerize(rang_bioc, output_dir = temp_dir) ## cache = FALSE
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^COPY cache", x)))
    expect_false(dir.exists(file.path(temp_dir, "cache")))
    expect_false(file.exists(file.path(temp_dir, "cache", "BiocGenerics_0.44.0.tar.gz")))
    expect_silent(dockerize(rang_bioc, output_dir = temp_dir, cache = TRUE, verbose = FALSE))
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache")))
    expect_true(file.exists(file.path(temp_dir, "cache", "BiocGenerics_0.44.0.tar.gz")))
})

test_that("issue 68, correct querying of bioc packages from major releases", {
    skip_if_offline()
    skip_on_cran()
    expect_equal(.query_biocver("2007-04-27")$version, "2.0")
    expect_false(is.numeric(.query_biocver("2007-04-27")$version))
    expect_error(.query_snapshot_dependencies_bioc("affy", "2007-04-15")) ## pre 2.0
    expect_error(.query_snapshot_dependencies_bioc("affy", "2007-04-27"), NA)
    expect_error(.query_snapshot_dependencies_bioc("affy", "2014-10-15"), NA)
    expect_equal(length(resolve("bioc::affy", snapshot_date = "2007-04-27")$ranglets), 1)
    expect_equal(length(resolve("bioc::affy", snapshot_date = "2014-10-15")$ranglets), 1)
})

test_that("issue 69, complete bioc information", {
    skip_if_offline()
    skip_on_cran()
    expect_true(nrow(.memo_search_bioc("3.3")) > 1211)
    expect_true("affydata" %in% .memo_search_bioc("3.3")$Package)
    expect_equal(.normalize_pkg("affydata", "3.3"), "bioc::affydata")
    expect_equal(length(resolve("bioc::affydata", snapshot_date = "2016-06-15")$ranglets), 1)
})

test_that("issue #82", {
    skip_if_offline()
    skip_on_cran()
    expect_error(x <- .query_snapshot_dependencies_bioc("zlibbioc", "2023-01-01"), NA)
    expect_false("y" %in% colnames(x))
    expect_error(x <- .query_snapshot_dependencies_bioc("Organism.dplyr", "2023-01-01"), NA)
    expect_true("y" %in% colnames(x))
})

test_that("issue #85", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve("restfulr", snapshot_date = "2023-01-01")
    expect_equal(x$ranglet[[1]]$unresolved_deps, character(0))
    expect_true("bioc::S4Vectors" %in% names(x$ranglet[[1]]$deps))
})

test_that("as_pkgrefs with bioc_version", {
    skip_if_offline()
    skip_on_cran()
    expect_equal(as_pkgrefs(c("rtoot", "S4Vectors"), bioc_version = "3.3"), c("cran::rtoot", "bioc::S4Vectors"))
})

test_that(".query_sysreqs_bioc with uncheckable info", {
    skip_if_offline()
    skip_on_cran()
    x <- .query_sysreqs_bioc("Rhtslib", "ubuntu-20.04")
    expect_true("apt-get install -y libbz2-dev" %in% x) ## uncheckable
    expect_true("apt-get install -y liblzma-dev" %in% x)
    expect_true("apt-get install -y make" %in% x) ## checkable
    x <- .query_sysreqs_bioc("Rhtslib", "centos-7")
    expect_true("yum install -y libbz2-devel" %in% x)
    expect_true("yum install -y xz-devel" %in% x)
    expect_true("yum install -y make" %in% x)
})

test_that("issue 89", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve("bioc::GenomeInfoDbData", snapshot_date = "2023-01-01")
    expect_equal(x$ranglets[["bioc::GenomeInfoDbData"]]$original$x_uid, "data/annotation")
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(x, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    expect_true(file.exists(file.path(temp_dir, "cache", "GenomeInfoDbData_1.2.9.tar.gz")))
})
