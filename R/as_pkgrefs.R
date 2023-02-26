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
    return(.normalize_pkgs(pkgs = x, bioc_version = bioc_version))
}

#' @rdname as_pkgrefs
#' @export
as_pkgrefs.sessionInfo <- function(x, ...) {
    vapply(X = x$otherPkgs, FUN = .extract_pkgref_packageDescription, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

.extract_pkgref_packageDescription <- function(packageDescription) {
    handle <- packageDescription[['Package']]
    if ("GithubRepo" %in% names(packageDescription)) {
        return(paste0("github::", packageDescription[["GithubUsername"]], "/", packageDescription[["GithubRepo"]]))
    }
    if (grepl("bioconductor", packageDescription[["URL"]])) {
      return(paste0("bioc::",handle))
    }
    ## uncomment this when #57 is implemented
    ##if (basename(attr(packageDescription, "file")) == "DESCRIPTION") {
        ## probably load via devtools::load_all
    ##    return(paste0("local::", dirname(attr(packageDescription, "file"))))
    ##}
    ## TODO bioc
    ## if (basename(attr(packageDescription, "file")) == "package.rds") {
    return(paste0("cran::", handle))
    ## }
}
