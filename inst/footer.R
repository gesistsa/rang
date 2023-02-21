current_r_version <- paste(R.Version()[c("major","minor")], collapse = ".", sep = "")

## In Unix, all things are file.
## Before you complain, R <= 3.2.0 doesn't have dir.exists.
if (file.exists("cache")) {
    path <- "cache"
} else {
    path <- tempdir()
}

if (nrow(installation_order) >= 1) {
    for (i in seq(from = 1, to = nrow(installation_order), by = 1)) {
        x <- installation_order$x[i]
        source <- installation_order$source[i]
        version <- installation_order$version[i]
        handle <- installation_order$handle[i]
        uid <- installation_order$uid[i]
        .install_from_source(x = x, version = version, handle = handle, source = source, uid = uid,
                             lib = lib, path = path, verbose = verbose,
                             cran_mirror = cran_mirror, bioc_mirror = bioc_mirror,
                             current_r_version = current_r_version) 
    }
}
