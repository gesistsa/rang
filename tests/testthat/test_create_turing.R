.generate_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

test_that("create_turing defensive", {
    existing_dir <- .generate_temp_dir()
    dir.create(existing_dir)
    expect_error(create_turing(existing_dir))
})

test_that("create_turing all cases", {
    temp_dir <- .generate_temp_dir()
    create_turing(temp_dir, verbose = FALSE) ## add_rang = TRUE
    expect_true(dir.exists(temp_dir))
    expect_true(file.exists(file.path(temp_dir, "CITATION")))
    expect_true(dir.exists(file.path(temp_dir, "data_raw")))
    expect_true(dir.exists(file.path(temp_dir, "data_clean")))
    expect_true(dir.exists(file.path(temp_dir, "figures")))
    expect_true(file.exists(file.path(temp_dir, "Makefile")))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang")))
    temp_dir <- .generate_temp_dir()
    create_turing(temp_dir, verbose = FALSE, add_makefile = FALSE) ## add_rang = TRUE
    expect_false(file.exists(file.path(temp_dir, "Makefile")))
    expect_true(dir.exists(temp_dir))
    expect_true(dir.exists(file.path(temp_dir, "data_raw")))
    expect_true(dir.exists(file.path(temp_dir, "data_clean")))
    expect_true(dir.exists(file.path(temp_dir, "figures")))
    expect_true(dir.exists(file.path(temp_dir, "inst/rang")))
    temp_dir <- .generate_temp_dir()
    create_turing(temp_dir, add_rang = FALSE)
    expect_true(dir.exists(temp_dir))
    expect_true(dir.exists(file.path(temp_dir, "data_raw")))
    expect_true(dir.exists(file.path(temp_dir, "data_clean")))
    expect_true(dir.exists(file.path(temp_dir, "figures")))
    expect_false(dir.exists(file.path(temp_dir, "inst/rang")))
})
