current_r_version <- paste(R.Version()[c("major","minor")], collapse = ".", sep = "")

if (length(install_order) >= 1) {
    for (i in seq(from = 1, to = length(install_order), by = 1)) {
        .install_from_cran(install_order[i], verbose = verbose, lib = lib, cran_mirror = cran_mirror,
                           current_r_version = current_r_version)
    }
}
