current_r_version <- paste(R.Version()[c("major","minor")], collapse = ".", sep = "")

## In Unix, all things are file.
## Before you complain, R <= 3.2.0 doesn't have dir.exists.
if (file.exists("cache")) {
    path <- "cache"
} else {
    path <- tempdir()
}

if (nrow(install_order) >= 1) {
    for (i in seq(from = 1, to = nrow(install_order), by = 1)) {
        x <- install_order$x[i]
        source <- install_order$source[i]
        version <- install_order$version[i]
        handle <- install_order$handle[i]
        uid <- install_order$uid[i]
        .install_from_source(x = x, version = version, handle = handle, source = source, uid = uid,
                             lib = lib, path = path, verbose = verbose,
                             cran_mirror = cran_mirror, current_r_version = current_r_version) 
    }
}
