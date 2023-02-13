## we follow:
## https://r-lib.github.io/pkgdepends/reference/pkg_refs.html

## syntax of a ref: source::handle
## source can be: "cran", "github" (as of now)
## a `handle` indicates how the `package` is sourced from the `source`:
## if source == "cran", handle <- package name as per DESCRIPTION, e.g. "rtoot"
## if source == "github", handle <- username/reponame, e.g. "schochastics/rtoot"

## Similar to pak::pak()
## `pkgs` parameter of resolve() can either be shorthands (e.g. "rtoot", "schochastics/rtoot")
## or pkgrefs (e.g. "cran::rtoot", "github::schochastics/rtoot")

## For `dep_df`
## compulsory columns
## `x`, `x_version`, `x_pubdate` are the information as per DESCRIPTION
## `x_pkgref` is x in pkgref, for internal storage, we don't use @ to indicate version / uid

## optional columns
## 1. if there are dependecies: `y`, `y_raw_version`, `type`, `y_pkgref`: as per DESCRIPTION; `y_raw_version` is not useful

## 2. for "github" (and possible "local" in the future)
## `x_uid` and `y_uid` are extra unique identifier for pinning down the package, if `?_version` isn't sufficient for this purpose
## if `source` == "github", `?_uid` <- "sha"

## `installation_order` should be an ordered data.frame
## not using snake case in column names for backward compatibility in containers, and not needed
## columns: x, version, source, handle, uid

.clean_suffixes <- function(pkgref) {
    ## remove all @, ?, or # suffixes, we don't support them
    gsub("[@#\\?].+", "", pkgref)
}

.parse_pkgref <- function(pkgref, return_handle = TRUE) {
    if (isFALSE(.is_pkgref(pkgref))) {
        stop(pkgref, "is not a valid `pkgref`", call. = FALSE)
    }
    ## remove all @, ?, or # suffixes, we don't support them
    pkgref <- .clean_suffixes(pkgref)
    res <- strsplit(pkgref, "::")[[1]]
    if (length(res) == 1) {
        source <- "cran"
        handle <- res[1]
    } else {
        source <- res[1]
        handle <- res[2]
    }
    if (isTRUE(return_handle)) {
        return(handle)
    }
    return(source)
}

.is_github <- function(pkg){
    ## For now, this is sufficient.
    ## If "local" and "url" are supported, this is not
    grepl("/", pkg)
}

.is_pkgref <- function(pkg) {
    grepl("::", pkg)
}

## TBI: .is_valid_pkgref
## pkgref is only valid if: exactly one "::", source %in% c("cran", "github"), if "github", .is_github is TRUE

.extract_github_handle <- function(url) {
    url <- gsub("^github::", "", url)
    if (isTRUE(grepl("@github\\.com", url))) {
        ## remote string
        info <- strsplit(url, ":")[[1]]
        return(gsub("\\.git$", "", info[2]))
    }
    info <- strsplit(url, "github\\.com")[[1]]
    path <- gsub("^/", "", info[length(info)])
    path_components <- strsplit(path, "/")[[1]]
    return(paste0(path_components[1], "/", path_components[2]))
}

## to normalize a pkg to pkgref
.normalize_pkg <- function(pkg) {
    if (pkg == "" || is.na(pkg)) {
        stop("Invalid `pkg`.", call. = FALSE)
    }
    if (isTRUE(.is_github(pkg))) {
        if (isTRUE(grepl("github\\.com", pkg))) {
            pkg <- .extract_github_handle(pkg)
        }
    }
    if (isTRUE(.is_pkgref(pkg))) {
        return(.clean_suffixes(pkg))
    }
    if (isTRUE(.is_github(pkg))) {
        return(paste0("github::", .clean_suffixes(pkg)))
    }
    return(paste0("cran::", .clean_suffixes(pkg)))
}

## vectorize
.normalize_pkgs <- function(pkgs) {
    vapply(X = pkgs, FUN = .normalize_pkg, FUN.VALUE = character(1), USE.NAMES = FALSE)
}