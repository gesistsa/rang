current.r.version <- paste(R.Version()[c("major","minor")], collapse = ".", sep = "")

## In Unix, all things are file.
## Before you complain, R <= 3.2.0 doesn't have dir.exists.
if (file.exists("cache")) {
    path <- "cache"
} else {
    path <- tempdir()
}

if (nrow(installation.order) >= 1) {
    for (i in seq(from = 1, to = nrow(installation.order), by = 1)) {
        x <- installation.order$x[i]
        source <- installation.order$source[i]
        version <- installation.order$version[i]
        handle <- installation.order$handle[i]
        uid <- installation.order$uid[i]
        .install.from.source(x = x, version = version, handle = handle, source = source, uid = uid,
                             lib = lib, path = path, verbose = verbose,
                             cran.mirror = cran.mirror, bioc.mirror = bioc.mirror,
                             current.r.version = current.r.version)
    }
}
