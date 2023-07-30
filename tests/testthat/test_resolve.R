.generate_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

skip_if(isTRUE(getOption("SKIP_RESOLVE")))

test_that("defensive programming", {
    expect_error(resolve("LDAvis", os = "windows"))
    expect_error(resolve("LDAvis", os = "opensuse-42.3"))
    expect_error(resolve("LDAvis", os = "sle-12.3"))
    expect_error(resolve("LDAvis", os = "sle-15.0"))
})

test_that(".extract_date", {
  expect_error(.extract_date("."), NA)
  expect_error(.extract_date("../testdata/renv.lock"), NA)
  expect_error(.extract_date("rtoot"), NA)
})

test_that(".check_local_in_pkgrefs", {
    expect_silent(.check_local_in_pkgrefs(c("cran::rtoot", "bioc::S4Vectors", "github::cran/rtoot")))
    expect_warning(.check_local_in_pkgrefs(c("local::../testdata/fakexml2")))
    expect_warning(.check_local_in_pkgrefs(c("local::../testdata/askpass_1.1.tar.gz")))
    expect_error(suppressWarnings(.check_local_in_pkgrefs(c("local::../testdata/issue39.RDS", "cran::rtoot"))))
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
    expect_false(file.exists(file.path(temp_dir, "cache/rpkgs", "LDAvis_0.3.2.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache/rpkgs", "proxy_0.4-27.tar.gz")))
    expect_false(file.exists(file.path(temp_dir, "cache/rpkgs", "RJSONIO_1.3-1.6.tar.gz")))
    expect_silent(dockerize(rang_ok, output_dir = temp_dir, cache = TRUE, verbose = FALSE))
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache/rpkgs")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "LDAvis_0.3.2.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "proxy_0.4-27.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "RJSONIO_1.3-1.6.tar.gz")))
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
    expect_true(dir.exists(file.path(temp_dir, "cache/rpkgs")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "rio_0.1.1.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "stringr_0.6.2.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "digest_0.6.3.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "foreign_0.8-54.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "evaluate_0.4.7.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "testthat_0.7.1.tar.gz")))
    ## Debian & rsrc
    expect_true(dir.exists(file.path(temp_dir, "cache/rsrc")))
    expect_true(file.exists(file.path(temp_dir, "cache/rsrc", "R-3.0.1.tar.gz")))
    expect_true(dir.exists(file.path(temp_dir, "cache/debian")))
    expect_true(file.exists(file.path(temp_dir, "cache/debian", "rootfs.tar.xz")))
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
    skip_if_offline()
    skip_on_cran()
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
    expect_false(dir.exists(file.path(temp_dir, "cache/rpkgs")))
    expect_false(file.exists(file.path(temp_dir, "cache/rpkgs", "BiocGenerics_0.44.0.tar.gz")))
    expect_silent(dockerize(rang_bioc, output_dir = temp_dir, cache = TRUE, verbose = FALSE))
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^COPY cache", x)))
    expect_true(dir.exists(file.path(temp_dir, "cache/rpkgs")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "BiocGenerics_0.44.0.tar.gz")))
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


test_that("issue 89", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve("bioc::GenomeInfoDbData", snapshot_date = "2023-01-01")
    expect_equal(x$ranglets[["bioc::GenomeInfoDbData"]]$original$x_uid, "data/annotation")
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(x, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "GenomeInfoDbData_1.2.9.tar.gz")))
})

test_that("integration of renv to resolve", {
    skip_if_offline()
    skip_on_cran()
    expect_error(X <- resolve("../testdata/small_renv_lock/renv.lock", snapshot_date = "2023-01-01"), NA)
})

test_that(".gh error handling", {
    skip_if_offline()
    skip_on_cran()
    expect_error(.gh("path/is/wrong"))
})

test_that(".query_snapshot_dependencies for local packages", {
    skip_if_offline()
    skip_on_cran()
    skip_on_os("windows") ## don't want to be slabbed in the back by Windows' paths
    expect_error(dep_df <- .query_snapshot_dependencies("local::../testdata/fakeRhtslib",
                                                   snapshot_date = "2023-01-01", bioc_version = "3.3"), NA)
    expect_true("y" %in% colnames(dep_df))
    expect_true("bioc::zlibbioc" %in% dep_df$y_pkgref)
    expect_true("cran::knitr" %in% dep_df$y_pkgref)
    expect_equal("Rhtslib", unique(dep_df$x))
    expect_true(grepl("^/", unique(dep_df$x_uid))) ## path expanded to abs. path
    ## do the same thing but with tar.gz
    expect_error(dep_df <- .query_snapshot_dependencies("local::../testdata/fakeRhtslib.tar.gz",
                                                   snapshot_date = "2023-01-01", bioc_version = "3.3"), NA)
    expect_true("y" %in% colnames(dep_df))
    expect_equal(unique(dep_df$x_pubdate), parsedate::parse_date("2023-01-01"))
    expect_true("bioc::zlibbioc" %in% dep_df$y_pkgref)
    expect_true("cran::knitr" %in% dep_df$y_pkgref)
    expect_equal("Rhtslib", unique(dep_df$x))
    expect_true(grepl("^/", unique(dep_df$x_uid))) ## path expanded to abs. path
    ## No y
    expect_error(dep_df <- .query_snapshot_dependencies("local::../testdata/fakezlibbioc",
                                                   snapshot_date = "2023-01-01", bioc_version = "3.3"), NA)
    expect_false("y" %in% colnames(dep_df))
    ## real data
    expect_error(dep_df <- .query_snapshot_dependencies("local::../testdata/askpass_1.1.tar.gz",
                                                   snapshot_date = "2023-01-01", bioc_version = "3.3"), NA)
    expect_true("cran::sys" %in% dep_df$y_pkgref)
})

test_that("dockerize local package as tarball", {
    skip_if_offline()
    skip_on_cran()
    temp_dir <- .generate_temp_dir()
    expect_error(suppressWarnings(graph <- resolve("local::../testdata/askpass_1.1.tar.gz", snapshot_date = "2023-01-01")), NA)
    expect_error(dockerize(graph, output_dir = temp_dir)) ## cache = FALSE
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA) ## cache = FALSE
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "sys_3.4.1.tar.gz")))
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "raw_askpass_1.1.tar.gz")))
})

test_that("dockerize local package as tarball", {
    skip_if_offline()
    skip_on_cran()
    temp_dir <- .generate_temp_dir()
    expect_error(suppressWarnings(graph <- resolve("local::../testdata/askpass", snapshot_date = "2023-01-01")), NA)
    expect_error(dockerize(graph, output_dir = temp_dir)) ## cache = FALSE
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    expect_true(file.exists(file.path(temp_dir, "cache/rpkgs", "sys_3.4.1.tar.gz")))
    expect_true(dir.exists(file.path(temp_dir, "cache/rpkgs", "dir_askpass_1.1")))
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^## ## WARNING", x)))
})

test_that("Rgraphviz", {
    skip_if_offline()
    skip_on_cran()
    x <- resolve("bioc::biocGraph", snapshot_date = "2023-01-01")
    expect_true("bioc::Rgraphviz" %in% names(x$ranglets[[1]]$deps))
})

test_that("dockerize R1.3.1 local", {
    skip_if_offline()
    skip_on_cran()
    expect_error(suppressWarnings(graph <- resolve("../testdata/sna_0.3.tar.gz", snapshot_date = "2001-09-11")), NA)
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA)
    x <- readLines(file.path(temp_dir, "rang.R"))
    expect_true(any(grepl("^## DEBUG INFO: CMD", x)))
})

test_that("skip_r17", {
    skip_if_offline()
    skip_on_cran()
    expect_error(suppressWarnings(graph <- resolve("../testdata/sna_0.3.tar.gz", snapshot_date = "2003-04-17")), NA)
    expect_equal(graph$r_version, "1.7.0")
    temp_dir <- .generate_temp_dir()
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE), NA) ## skip_r17 = TRUE
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true(any(grepl("^RUN bash \\$COMPILE_PATH 1\\.8\\.0", x)))
    expect_error(dockerize(graph, output_dir = temp_dir, cache = TRUE, verbose = FALSE, skip_r17 = FALSE), NA)
    x <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_false(any(grepl("^RUN bash \\$COMPILE_PATH 1\\.8\\.0", x)))
    expect_true(any(grepl("^RUN bash \\$COMPILE_PATH 1\\.7\\.0", x)))
    ## debian & rsrc
    expect_true(dir.exists(file.path(temp_dir, "cache/rsrc")))
    expect_true(file.exists(file.path(temp_dir, "cache/rsrc", "R-1.7.0.tgz")))
    expect_true(dir.exists(file.path(temp_dir, "cache/debian")))
    expect_true(file.exists(file.path(temp_dir, "cache/debian", "rootfs.tar.xz")))
})

test_that("dockerize with inst/rang", {
    skip_if_offline()
    skip_on_cran()
    warns2 <- capture_warnings(rang_ok <- resolve(c("sna"), snapshot_date = "2001-10-01"))
    temp_dir <- .generate_temp_dir()
    dir.create(temp_dir)
    use_rang(temp_dir, verbose = FALSE)
    dockerize(rang_ok, output_dir = temp_dir, verbose = FALSE, cache = TRUE)
    expect_true("inst/rang/rang.R" %in% list.files(temp_dir, recursive = TRUE))
    expect_false("rang.R" %in% list.files(temp_dir, recursive = TRUE))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang/cache")))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang/cache/rpkgs")))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang/cache/debian")))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang/cache/rsrc")))
    expect_true("Dockerfile" %in% list.files(temp_dir, recursive = TRUE))
    dockerfile <- readLines(file.path(temp_dir, "Dockerfile"))
    expect_true("COPY . /" %in% dockerfile) ## coerced
})


## always keep this at the very last

test_that("issue 102 confusion between github and local pkgref", {
    skip_if_offline()
    skip_on_cran()
    original_wd <- getwd()
    setwd(normalizePath(file.path("../testdata")))
    expect_error(suppressWarnings(graph <- resolve("local::./askpass", snapshot_date = "2023-01-01")), NA)
    expect_equal(.parse_pkgref(unique(graph$ranglets[[1]]$original$x_pkgref), FALSE), "local")
    setwd(original_wd)
})
