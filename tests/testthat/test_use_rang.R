.generate_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

expect_unequal <- function(x, y) {
    expect_false(isTRUE(all.equal(x, y)))
}

test_that("use_rang defensive", {
    expect_error(use_rang("this directory does not exist"))
})

test_that("use_rang normal", {
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    msg <- capture_messages(use_rang(tempdir))
    expect_true(any(grepl("infrastructure", msg)))
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    expect_silent(use_rang(tempdir, verbose = FALSE))
    expect_true(dir.exists(file.path(tempdir, "inst/rang")))
    expect_true(file.exists(file.path(tempdir, "inst/rang/update.R")))
    expect_true(file.exists(file.path(tempdir, ".here")))
    expect_true(file.exists(file.path(tempdir, "Makefile")))
})

test_that("options", {
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    use_rang(tempdir, add_makefile = FALSE, verbose = FALSE)
    expect_true(dir.exists(file.path(tempdir, "inst/rang")))
    expect_true(file.exists(file.path(tempdir, "inst/rang/update.R")))
    expect_true(file.exists(file.path(tempdir, ".here")))
    expect_false(file.exists(file.path(tempdir, "Makefile")))
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    use_rang(tempdir, add_here = FALSE, verbose = FALSE)
    expect_true(dir.exists(file.path(tempdir, "inst/rang")))
    expect_true(file.exists(file.path(tempdir, "inst/rang/update.R")))
    expect_false(file.exists(file.path(tempdir, ".here")))
    expect_true(file.exists(file.path(tempdir, "Makefile")))
})

test_that("Existing components update.R", {
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    dir.create(file.path(tempdir, "inst/rang"), recursive = TRUE)
    dummy <- c("831721", "GESIS")
    writeLines(dummy, file.path(tempdir, "inst/rang/update.R"))
    use_rang(tempdir, verbose = FALSE)
    content <- readLines(file.path(tempdir, "inst/rang/update.R"))
    expect_equal(content, dummy)
    ## force
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    dir.create(file.path(tempdir, "inst/rang"), recursive = TRUE)
    writeLines(dummy, file.path(tempdir, "inst/rang/update.R"))
    use_rang(tempdir, verbose = FALSE, force = TRUE)
    content <- readLines(file.path(tempdir, "inst/rang/update.R"))
    expect_unequal(content, dummy) ## got overwritten
})

test_that("Existing components Makefile", {
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    dummy <- c("831721", "GESIS")
    writeLines(dummy, file.path(tempdir, "Makefile"))
    use_rang(tempdir, verbose = FALSE)
    content <- readLines(file.path(tempdir, "Makefile"))
    expect_equal(content, dummy)
    ## force
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    writeLines(dummy, file.path(tempdir, "Makefile"))
    use_rang(tempdir, verbose = FALSE, force = TRUE)
    content <- readLines(file.path(tempdir, "Makefile"))
    expect_unequal(content, dummy) ## got overwritten
})

test_that("Existing components .here", {
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    dummy <- c("831721", "GESIS")
    writeLines(dummy, file.path(tempdir, ".here"))
    use_rang(tempdir, verbose = FALSE)
    content <- readLines(file.path(tempdir, ".here"))
    expect_equal(content, dummy)
    ## force
    tempdir <- .generate_temp_dir()
    dir.create(tempdir)
    writeLines(dummy, file.path(tempdir, ".here"))
    use_rang(tempdir, verbose = FALSE, force = TRUE)
    content <- readLines(file.path(tempdir, ".here"))
    expect_unequal(content, dummy) ## got overwritten
})
