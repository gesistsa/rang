#' Convert Data Structures into Package References
#'
#' This generic function converts several standard data structures into a vector of package references, which in turn
#' can be used as the first argument of the function [resolve()]. This function guessimates the possible sources of the
#' packages. But we strongly recommend manually reviewing the detected packages before using them for [resolve()].
#' @param x, currently supported data structure(s) are: output from [sessionInfo()], a character vector of package names
#' @param bioc_version character. When x is a character vector, version of Bioconductor to search for package names. NULL indicates not
#' search for Bioconductor.
#' @param ..., not used
#' @return a vector of package references
#' @export
#' @examples
#' as_pkgrefs(sessionInfo())
#' if (interactive()) {
#'    require(rang)
#'    graph <- resolve(as_pkgrefs(sessionInfo()))
#'    as_pkgrefs(c("rtoot"))
#'    as_pkgrefs(c("rtoot", "S4Vectors")) ## this gives cran::S4Vectors and is not correct.
#'    as_pkgrefs(c("rtoot", "S4Vectors"), bioc_version = "3.3") ## This gives bioc::S4Vectors
#' }
as_pkgrefs <- function(x, ...) {
    UseMethod("as_pkgrefs", x)
}

#' @rdname as_pkgrefs
#' @export
as_pkgrefs.default <- function(x, ...) {
    ## an exported version of .normalize_pkgs
    ## if (is.numeric(x) || is.logical(x) || is.integer(x)) {
    stop("Don't know how to convert this to package references.", call. = FALSE)
    ## }
    ## return(.normalize_pkgs(x))
}

#' @rdname as_pkgrefs
#' @export
as_pkgrefs.character <- function(x, bioc_version = NULL, ...) {
    if(.is_renv_lockfile(x)) {
      return(.extract_pkgrefs_renv_lockfile(path = x))
    }
    if(.is_directory(x)) {
      return(.extract_pkgrefs_dir(x,bioc_version))
    }
    return(.normalize_pkgs(pkgs = x, bioc_version = bioc_version))
}

#' @rdname as_pkgrefs
#' @export
as_pkgrefs.sessionInfo <- function(x, ...) {
    vapply(X = x$otherPkgs, FUN = .extract_pkgref_packageDescription, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

.extract_pkgrefs_renv_lockfile <- function(path) {
    lockfile <- .parse_renv_lockfile(path)
    sources <- vapply(lockfile[["Packages"]],`[[`,character(1),"Source",USE.NAMES = FALSE)
    pkgs <- c()
    if("Repository" %in% sources) {
        pkgs <- c(pkgs, paste0("cran::",vapply(lockfile[["Packages"]][sources=="Repository"],`[[`,character(1),"Package",USE.NAMES = FALSE)))
    }
    if("Bioconductor" %in% sources) {
        pkgs <- c(pkgs,paste0("bioc::",vapply(lockfile[["Packages"]][sources=="Bioconductor"],`[[`,character(1),"Package",USE.NAMES = FALSE)))
    }
    if("GitHub" %in% sources) {
        pkgs <- c(pkgs,
                  paste0("github::",
                         vapply(lockfile[["Packages"]][sources=="GitHub"],`[[`,character(1), "RemoteUsername", USE.NAMES = FALSE),"/",
                         vapply(lockfile[["Packages"]][sources=="GitHub"],`[[`,character(1), "Package", USE.NAMES = FALSE))
                  )
    }
    if ("Local" %in% sources) {
        pkgs <- c(pkgs, paste0("local::", vapply(lockfile[["Packages"]][sources=="Local"],`[[`,character(1),"RemoteUrl",USE.NAMES = FALSE)))
    }
    return(pkgs)
}

.extract_pkgref_packageDescription <- function(packageDescription) {
    handle <- packageDescription[['Package']]
    if ("GithubRepo" %in% names(packageDescription)) {
        return(paste0("github::", packageDescription[["GithubUsername"]], "/", packageDescription[["GithubRepo"]]))
    }
    if (grepl("bioconductor", packageDescription[["URL"]])) {
      return(paste0("bioc::",handle))
    }
    if (basename(attr(packageDescription, "file")) == "DESCRIPTION") {
        ## probably load via devtools::load_all
        return(paste0("local::", dirname(attr(packageDescription, "file"))))
    }
    return(paste0("cran::", handle))
}

.is_renv_lockfile <- function(path) {
    # assuming all renv lockfiles are called renv.lock and path is only length 1
    if(length(path)!=1) {
      return(FALSE)
    }
    if(isFALSE(file.exists(path))) {
      return(FALSE)
    }
    if (isFALSE(basename(path) == "renv.lock")) {
      return(FALSE)
    }
    TRUE
}

.parse_renv_lockfile <- function(path) {
    lockfile <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    # class(lockfile) <- "renv_lockfile"
    lockfile
}

.is_directory <- function(path) {
    if(length(path)!=1) {
      return(FALSE)
    }
    if(isFALSE(dir.exists(path))) {
      return(FALSE)
    }
    TRUE
}

.extract_pkgrefs_dir <- function(path, bioc_version = NULL) {
    pkgs <- suppressMessages(unique(renv::dependencies(path,progress = FALSE)$Package))
    warning("scanning directories for R packages cannot detect github packages.",call. = FALSE)
    return(.normalize_pkgs(pkgs = pkgs, bioc_version = bioc_version))
}
