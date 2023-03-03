## all are online tests

test_that(".query_singleline_sysreqs", {
    skip_if_offline()
    skip_on_cran()
    res <- .query_singleline_sysreqs("")
    expect_equal(res, character(0))
    res <- .query_singleline_sysreqs("tensorflow")
    expect_equal(res, character(0))
    res <- .query_singleline_sysreqs("GNU Scientific Library version >= 1.8, C++11", "ubuntu-20.04") ## cheat
    expect_equal(res, "apt-get install -y libgsl0-dev")
    res <- .query_singleline_sysreqs("Tcl/Tk", "ubuntu-20.04") ## uncheckable
    expect_equal(res, "apt-get install -y tcl8.6 tk8.6") ## cheat
})

test_that(".query_sysreqs_github", {
    skip_if_offline()
    skip_on_cran()
    ## This doesn't query for system requirements of deep dependencies anymore
    res <- .query_sysreqs_github("cran/topicmodels", os = "ubuntu-20.04")
    expect_true(all(grepl("^apt-get", res)))
    expect_true(length(res) == 1)
    res <- .query_sysreqs_github("cran/topicmodels", "centos-8")
    expect_true(all(grepl("^dnf", res)))
    res <- .query_sysreqs_github("Bioconductor/Rhtslib", "ubuntu-20.04")
    res2 <- .query_sysreqs_bioc("Rhtslib", "ubuntu-20.04")
    expect_equal(res, res2)
})

test_that(".query_sysreqs_bioc with uncheckable info", {
    skip_if_offline()
    skip_on_cran()
    x <- .query_sysreqs_bioc("Rhtslib", "ubuntu-20.04")
    expect_true("apt-get install -y libbz2-dev" %in% x) ## uncheckable
    expect_true("apt-get install -y liblzma-dev" %in% x)
    expect_true("apt-get install -y make" %in% x) ## checkable
    expect_false("apt-get install -y" %in% x) ## the null response from C++
    x <- .query_sysreqs_bioc("Rhtslib", "centos-7")
    expect_true("dnf install -y libbz2-devel" %in% x)
    expect_true("dnf install -y xz-devel" %in% x)
    expect_true("dnf install -y make" %in% x)
    expect_false("dnf install -y" %in% x) ## the null response from C++
    x <- .query_singleline_sysreqs("libxml2", "ubuntu-20.04")
    expect_equal(x, "apt-get install -y libxml2-dev")
    x <- .query_singleline_sysreqs("C++", "ubuntu-20.04")
    expect_equal(x, character(0))
    x <- readRDS("../testdata/sysreqs_gmp.RDS")
    ## buildtime / runtime requirements
    expect_equal(.extract_sys_package(x[[1]], arch = "DEB"),
                 "apt-get install -y libgmp-dev")
})

test_that(".query_sysreqs_local", {
    skip_if_offline()
    skip_on_cran()
    expect_error(sysreqs <- .query_sysreqs_local(c("../testdata/fakexml2", "../testdata/askpass_1.1.tar.gz", "../testdata/fakeRhtslib.tar.gz"), "ubuntu-20.04"), NA)
    expect_true("apt-get install -y libxml2-dev" %in% sysreqs)
    expect_true("apt-get install -y libbz2-dev" %in% sysreqs)
    ## dispatch in .query_sysreqs_smart
    expect_error(sysreqs2 <- .query_sysreqs_smart(c("local::../testdata/fakexml2", "local::../testdata/askpass_1.1.tar.gz", "local::../testdata/fakeRhtslib.tar.gz"), "ubuntu-20.04"), NA)
    expect_equal(sysreqs, sysreqs2)
})
