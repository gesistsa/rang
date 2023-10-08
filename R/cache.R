#' @importFrom utils download.file
#' @importFrom here here
NULL

.query_mirror_validity <- function(mirror) {
    if (mirror == "https://cran.r-project.org/") {
        return(TRUE)
    }
    all_mirrors <- utils::getCRANmirrors()$URL
    mirror %in% all_mirrors
}

.normalize_url <- function(mirror, https = TRUE) {
    if (grepl("^http://", mirror)) {
        mirror <- gsub("^http://", "https://", mirror)
    }
    if (!grepl("^https://", mirror)) {
        mirror <- paste0("https://", mirror)
    }
    if (!grepl("/$", mirror)) {
        mirror <- paste0(mirror, "/")
    }
    if (grepl("/+$", mirror)) {
        mirror <- gsub("/+$", "/", mirror)
    }
    if (isTRUE(https)) {
        return(mirror)
    } else {
        return(gsub("^https://", "http://", mirror))
    }
}

.check_tarball_path <- function(tarball_path, x, dir = FALSE) {
    ## raise error when tarball_path doesn't exist
    if ((isFALSE(dir) && isFALSE(file.exists(tarball_path))) ||
        (isTRUE(dir) && isFALSE(dir.exists(tarball_path)))) {
        stop(x, " can't be cached.", call. = FALSE)
    }
    invisible()
}

.cache_pkg_cran <- function(x, version, cache_dir, cran_mirror, verbose) {
    url <- paste(cran_mirror, "src/contrib/Archive/", x, "/", x, "_", version, ".tar.gz", sep = "")
    tarball_path <- file.path(cache_dir, paste(x, "_", version, ".tar.gz", sep = ""))
    tryCatch({
        suppressWarnings(utils::download.file(url, destfile = tarball_path, quiet = !verbose))
    }, error = function(e) {
        ## is the current latest
        url <- paste(cran_mirror, "src/contrib/", x, "_", version, ".tar.gz", sep = "")
        utils::download.file(url, destfile = tarball_path, quiet = !verbose)
    })
    .check_tarball_path(tarball_path, x)
}

.cache_pkg_bioc <- function(x, version, cache_dir, bioc_mirror, bioc_version, verbose, uid) {
    url <- paste(bioc_mirror, bioc_version, "/", uid, "/src/contrib/", x, "_", version, ".tar.gz", sep = "")
    tarball_path <- file.path(cache_dir, paste(x, "_", version, ".tar.gz", sep = ""))
    suppressWarnings(utils::download.file(url, destfile = tarball_path, quiet = !verbose))
    .check_tarball_path(tarball_path, x)
}

.cache_pkg_github <- function(x, version, handle, source, uid, cache_dir, verbose) {
    sha <- uid
    tarball_path <- file.path(cache_dir, paste("raw_", x, "_", version, ".tar.gz", sep = ""))
    utils::download.file(paste("https://api.github.com/repos/", handle, "/tarball/", sha, sep = ""), destfile = tarball_path,
                         quiet = !verbose)
    .check_tarball_path(tarball_path, x)
}

.cache_pkg_local <- function(x, version, cache_dir, uid) {
    local_path <- uid
    tarball_path <- file.path(cache_dir, paste("raw_", x, "_", version, ".tar.gz", sep = ""))
    if (isTRUE(grepl("\\.tar.gz$|\\.tgz$", local_path))) {
        ## it could be a valid source package, but don't trust it blindly, mark it as raw_
        ## similar to github packages
        file.copy(local_path, tarball_path)
        return(.check_tarball_path(tarball_path, x))
    }
    if (.is_directory(local_path)) {
        dir_pkg_path <- file.path(cache_dir, paste("dir_", x, "_", version, sep = ""))
        res <- file.copy(from = local_path, to = cache_dir, recursive = TRUE, overwrite = TRUE)
        res <- file.rename(from = file.path(cache_dir, x), to = dir_pkg_path)
        return(.check_tarball_path(dir_pkg_path, x, dir = TRUE))
    }
}

.cache_pkgs <- function(rang, base_dir, cran_mirror, bioc_mirror, verbose) {
    installation_order <- generate_installation_order(rang)
    cache_dir <- file.path(base_dir, "cache", "rpkgs")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    for (i in seq(from = 1, to = nrow(installation_order), by = 1)) {
        x <- installation_order$x[i]
        source <- installation_order$source[i]
        version <- installation_order$version[i]
        handle <- installation_order$handle[i]
        uid <- installation_order$uid[i]
        if (source == "cran") {
            .cache_pkg_cran(x = x, version = version, cache_dir = cache_dir,
                        cran_mirror = cran_mirror, verbose = verbose)
        }
        if (source == "github") {
            ## please note that these cached packages are not built
            .cache_pkg_github(x = x, version = version, handle = handle,
                          source = source, uid = uid,
                          cache_dir = cache_dir, verbose = verbose)
        }
        if(source == "bioc") {
            .cache_pkg_bioc(x = x, version = version, cache_dir = cache_dir,
                            bioc_mirror = bioc_mirror, bioc_version = rang$bioc_version, verbose = verbose,
                            uid = uid)
        }
        if(source == "local") {
            ## please note that these cached packages are not built
            .cache_pkg_local(x = x, version = version, cache_dir = cache_dir, uid = uid)
        }
    }
    invisible(base_dir)
}

.cache_rsrc <- function(r_version, base_dir, verbose, cran_mirror) {
    cache_dir <- file.path(base_dir, "cache", "rsrc")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    major_version <- as.character(package_version(r_version)$major)
    if (major_version == "1") {
        file_extension <- ".tgz"
    } else {
        file_extension <- ".tar.gz"
    }
    download_dir <- paste0("R-", major_version)
    tar_file <- paste0("R-", r_version, file_extension)
    url <- paste0(cran_mirror, "src/base/", download_dir, "/", tar_file)
    tar_path <- file.path(cache_dir, tar_file)
    download.file(url = url, destfile = tar_path, quiet = !verbose)
    if (!file.exists(tar_path)) {
        stop("Fail to cache R source.")
    }
    return(tar_path)
}


.cache_debian <- function(debian_version, base_dir, verbose) {
    cache_dir <- file.path(base_dir, "cache", "debian")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    debian_image_url <- debian_urls[debian_version]
    rootfs_path <- file.path(cache_dir, "rootfs.tar.xz")
    download.file(debian_image_url, destfile = rootfs_path, quiet = !verbose)
    if (!file.exists(rootfs_path)) {
        stop("Fail to cache Debian disk image.")
    }
    return(rootfs_path)
}
