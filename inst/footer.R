current_r_version <- paste(R.Version()[c("major","minor")], collapse = ".", sep = "")

if (dir.exists("cache")) {
    path <- "cache"
} else {
    path <- tempdir()
}

if (length(install_order) >= 1) {
    for (i in seq(from = 1, to = length(install_order), by = 1)) {
        .install_from_cran(x = install_order[i], lib = lib, path = path, verbose = verbose,
                           cran_mirror = cran_mirror, current_r_version = current_r_version)
    }
}
