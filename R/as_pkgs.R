#' @export
as_pkgs <- function(x, ...) {
    UseMethod("as_pkgs", x)
}

as_pkgs.sessionInfo <- function(x, ...) {
    vapply(X = x$otherPkgs, FUN = .extract_pkgref_packageDescription, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

.extract_pkgref_packageDescription <- function(packageDescription) {
    handle <- packageDescription[['Package']]
    if ("GithubRepo" %in% names(packageDescription)) {
        return(paste0("github::", packageDescription[["GithubUsername"]], "/", packageDescription[["GithubRepo"]]))
    }
    if (basename(attr(packageDescription, "file")) == "DESCRIPTION") {
        ## probably load via devtools::load_all
        return(paste0("local::", dirname(attr(packageDescription, "file"))))
    }
    if (basename(attr(packageDescription, "file")) == "package.rds") {
        return(paste0("cran::", handle))
    }
}
