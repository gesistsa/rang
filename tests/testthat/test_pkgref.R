## using all the cases in https://r-lib.github.io/pkgdepends/reference/pkg_refs.html

## .normalize_pkgs aka as_pkgrefs

test_that(".normalize_pkgs, defensive programming", {
    expect_error(.normalize_pkgs(c("forecast", "r-lib/crayon")), NA)
    expect_error(.normalize_pkgs(NULL), NA)
    expect_error(.normalize_pkgs(c()), NA)
    expect_error(.normalize_pkgs())
    expect_error(.normalize_pkgs(c("forecast", "")))
    expect_error(.normalize_pkgs(c("forecast", NA)))
    expect_error(.normalize_pkgs(c("forecast", NA)))
    expect_error(.normalize_pkgs(NA))
    expect_error(.normalize_pkgs(""))
})

test_that(".normalize_pkgs: cran", {
    expect_equal(.normalize_pkgs("cran::testthat?source&nocache"), "cran::testthat")
    expect_equal(.normalize_pkgs("cran::testthat?source=true&nocache=true"), "cran::testthat")
    expect_equal(.normalize_pkgs("forecast"), "cran::forecast")
    expect_equal(.normalize_pkgs("forecast@8.8"), "cran::forecast")
    expect_equal(.normalize_pkgs("forecast@>=8.8"), "cran::forecast")
    expect_equal(.normalize_pkgs("cran::forecast"), "cran::forecast")
    expect_equal(.normalize_pkgs("forecast@last"), "cran::forecast")
    expect_equal(.normalize_pkgs("forecast@current"), "cran::forecast")
})

test_that(".normalize_pkgs: github", {
    expect_equal(.normalize_pkgs("r-lib/crayon"), "github::r-lib/crayon")
    expect_equal(.normalize_pkgs("github::r-lib/crayon"), "github::r-lib/crayon")
    expect_equal(.normalize_pkgs("r-lib/crayon@84be6207"), "github::r-lib/crayon")
    expect_equal(.normalize_pkgs("r-lib/crayon@branch"), "github::r-lib/crayon")
    expect_equal(.normalize_pkgs("r-lib/crayon#41"), "github::r-lib/crayon")
    expect_equal(.normalize_pkgs("r-lib/crayon@release"), "github::r-lib/crayon")
})

test_that(".normalize_pkgs: github url", {
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr/tree/ghactions"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr/tree/v2.1.1"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr/commit/8fbcb548e316"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr/pull/76"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("https://github.com/r-lib/withr/releases/tag/v2.1.0"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr/tree/ghactions"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr/tree/v2.1.1"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr/commit/8fbcb548e316"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr/pull/76"), "github::r-lib/withr")
    expect_equal(.normalize_pkgs("github::https://github.com/r-lib/withr/releases/tag/v2.1.0"), "github::r-lib/withr")
})

test_that(".normalize_pkgs: github remote string", {
    expect_equal(.normalize_pkgs("git@github.com:r-lib/pak.git"), "github::r-lib/pak")
    expect_equal(.normalize_pkgs("github::git@github.com:r-lib/pak.git"), "github::r-lib/pak")
})

test_that(".normalize_pkgs: local", {
    expect_equal(.normalize_pkgs("local::/foo/bar/package_1.0.0.tar.gz"), "local::/foo/bar/package_1.0.0.tar.gz")
    expect_equal(.normalize_pkgs("local::/foo/bar/pkg"), "local::/foo/bar/pkg")
    expect_equal(.normalize_pkgs("local::."), "local::.")
    expect_equal(.normalize_pkgs("/absolute/path/package_1.0.0.tar.gz"), "local::/absolute/path/package_1.0.0.tar.gz")
    expect_equal(.normalize_pkgs("~/path/from/home"), "local::~/path/from/home")
    expect_equal(.normalize_pkgs("./relative/path"), "local::./relative/path")
    expect_equal(.normalize_pkgs("."), "local::.")
})

test_that("as_pkgrefs dispatch", {
    expect_error(as_pkgrefs(TRUE))
    expect_error(as_pkgrefs(7.21))
    expect_error(as_pkgrefs(1L))
    expect_equal(as_pkgrefs("rtoot"), "cran::rtoot")
    expect_equal(as_pkgrefs(c("rtoot", "sna")), c("cran::rtoot", "cran::sna"))
    expect_equal(as_pkgrefs(c("rtoot", "S4Vectors")), c("cran::rtoot", "cran::S4Vectors")) ## the bioc version is in test_resolve
})

## .parse_pkgref

test_that(".parse_pkgref", {
    expect_error(.parse_pkgref("withr"))
    expect_error(.parse_pkgref("r-lib/withr"))
    expect_equal(.parse_pkgref("github::r-lib/withr", TRUE), "r-lib/withr")
    expect_equal(.parse_pkgref("github::r-lib/withr@123", TRUE), "r-lib/withr")
    expect_equal(.parse_pkgref("github::r-lib/withr", FALSE), "github")
    expect_equal(.parse_pkgref("github::r-lib/withr@123", FALSE), "github")
    expect_equal(.parse_pkgref("cran::testthat?source&nocache", TRUE), "testthat")
    expect_equal(.parse_pkgref("cran::testthat?source&nocache", FALSE), "cran")
    expect_equal(.parse_pkgref("cran::testthat", TRUE), "testthat")
    expect_equal(.parse_pkgref("cran::testthat", FALSE), "cran")
    expect_equal(.parse_pkgref("local::./relative/path", TRUE), "./relative/path")
    expect_equal(.parse_pkgref("local::./relative/path", FALSE), "local")
})

## as_pkgrefs.sessionInfo()

test_that(".extract_pkgref_packageDescription", {
    si <- readRDS("../testdata/sessionInfo1.RDS")
    expect_equal(.extract_pkgref_packageDescription(si$otherPkgs[[1]]), "github::chainsawriot/grafzahl")
    expect_equal(.extract_pkgref_packageDescription(si$otherPkgs[[2]]), "cran::rtoot")
    expect_equal(.extract_pkgref_packageDescription(si$otherPkgs[[3]]), "local::/home/chainsawriot/dev/rang")
    expect_equal(.extract_pkgref_packageDescription(si$otherPkgs[[4]]), "cran::testthat")
})

test_that("as_pkgrefs_packageDescription", {
    si <- readRDS("../testdata/sessionInfo1.RDS")
    res <- as_pkgrefs(si)
    expect_equal(res, c("github::chainsawriot/grafzahl", "cran::rtoot", "local::/home/chainsawriot/dev/rang", "cran::testthat"))
    ## bioc
    si <- readRDS("../testdata/sessionInfo3.RDS")
    res <- as_pkgrefs(si)
    expect_true("bioc::BiocGenerics" %in% res)
    expect_true("bioc::S4Vectors" %in% res)
})

## as_pkgregs.character (renv)

test_that("as_pkgrefs renv_lockfile", {
    res <- as_pkgrefs("../testdata/large_renv_lock/renv.lock")
    expect_equal(res, readRDS("../testdata/bioc_renv.RDS"))
})

test_that(".is_renv_lockfile false",{
    expect_false(.is_renv_lockfile("./testdata/graph.RDS"))
    expect_false(.is_renv_lockfile(c("../testdata/graph.RDS", "../testdata/renv.lock")))
    expect_false(.is_renv_lockfile("../testdata/fake_renv.lock"))
})

test_that("as_pkgrefs renv_lockfile with local", {
    res <- as_pkgrefs("../testdata/local_renv_lock/renv.lock")
    expect_true("local::~/dev/rang/tests/testdata/askpass_1.1.tar.gz" %in% res)
    expect_true("local::~/dev/rang" %in% res)
})

## as_pkgrefs.character (directory -> scanning)

test_that("as_pkgrefs directory", {
    skip_if_offline()
    skip_on_cran()
    res <- suppressWarnings(as_pkgrefs("../testdata/test_dir",bioc_version = "3.16"))
    expect_equal(res, c("bioc::BiocGenerics", "cran::rtoot"))
})

## as_pkgrefs.character (DESCRIPTION)
test_that("as_pkgrefs DESCRIPTION", {
    res <- suppressWarnings(as_pkgrefs("../testdata/DESCRIPTION",bioc_version = "3.16"))
    expect_equal(res, c("cran::xaringan", "cran::xaringanExtra", "cran::leaflet", "cran::fontawesome", 
                        "github::yihui/xaringan", "github::chainsawriot/xaringanExtra", 
                        "github::rstudio/fontawesome"))
})



## .is_*

test_that(".is_pkgref", {
    expect_true(.is_pkgref("cran::rtoot"))
    expect_false(.is_pkgref("cran::"))
    expect_false(.is_pkgref("cran:::"))
    expect_false(.is_pkgref("xran:::"))
})

test_that(".is_github", {
    expect_true(.is_github("cran/rtoot"))
    expect_true(.is_github("https://github.com/cran/rtoot"))
    expect_true(.is_github("https://www.github.com/cran/rtoot"))
    expect_true(.is_github("git@github.com:r-lib/pak.git"))
    expect_false(.is_github("cran//rtoot"))
    expect_false(.is_github("~/hello"))
    expect_false(.is_github("./hello"))
    expect_false(.is_github("/hello"))
    expect_false(.is_github("/hello/world"))
    expect_false(.is_github("/hello/world/"))
    expect_false(.is_github("world/"))
})

test_that(".is_directory false",{
    expect_false(.is_directory(c("a/","b/")))
    expect_false(.is_directory("a/"))
})

test_that(".is_local", {
    expect_false(.is_local("cran/rtoot"))
    expect_false(.is_local("cran//rtoot"))
    expect_false(.is_local("world/"))
    expect_true(.is_local("~/hello"))
    expect_true(.is_local("./hello"))
    expect_true(.is_local("/hello"))
    expect_true(.is_local("/hello/world"))
    expect_true(.is_local("/hello/world/"))
    expect_true(.is_local("/hello/world/"))
    expect_true(.is_local("../testdata/fakexml2"))
})

test_that(".is_local precedes .is_github", {
    expect_false(.is_github("~/helloworld"))
    expect_false(.is_github("./helloworld"))
})
