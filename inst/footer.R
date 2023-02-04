
for (i in seq_along(install_order)) {
    .install_from_cran(install_order[i], verbose = verbose, lib = lib)
}
