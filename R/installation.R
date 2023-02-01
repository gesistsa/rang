##granlist <- readRDS("tests/testdata/graph.RDS")

.determine_installation_order <- function(granlist) {
    dep <- fastmap::fastmap()
    version <- fastmap::fastmap()
    for (gran in granlist$grans) {
        current_pkg <- unique(gran$original$x)
        current_ver <- unique(gran$original$x_version)
        current_dep <- .keep_queryable_dependencies(dep_df = gran$original, no_enhances = gran$no_enhances, no_suggests = gran$no_suggests)
        dep$set(current_pkg, current_dep)
        version$set(current_pkg, current_ver)
        for (dep_df in gran$deps) {
            current_pkg <- unique(dep_df$x)
            current_ver <- unique(dep_df$x_version)
            current_dep <- .keep_queryable_dependencies(dep_df = dep_df, no_enhances = gran$no_enhances, no_suggests = gran$no_suggests)
            dep$set(current_pkg, current_dep)
            version$set(current_pkg, current_ver)
        }
    }
    ## installation simulation
    installed_packages <- c()
    needed_packages <- dep$keys()
    ## install all terminal nodes
    for (package in needed_packages) {
        if (is.null(dep$get(package))) {
            installed_packages <- c(installed_packages, package)
        }
    }
    while(length(setdiff(needed_packages, installed_packages)) != 0) {
        for (package in needed_packages) {
            ##print(package)
            if (!package %in% installed_packages) {
                ## check requirement
                requirement_fulfilled <- length(setdiff(dep$get(package), installed_packages)) == 0
                if (requirement_fulfilled) {
                    installed_packages <- c(installed_packages, package)
                }
            }
        }
    }
    vapply(installed_packages, function(x) version$get(x), character(1))
}

## .install_from_cran <- function(x, lib, path = tempdir()) {
##     url <- paste0("https://cran.r-project.org/src/contrib/Archive/", names(x), "/", names(x), "_", x, ".tar.gz")
##     tarball_path <- file.path(path, paste0(names(x), "_", x, ".tar.gz"))
##     tryCatch({
##         suppressWarnings(utils::download.file(url, destfile = tarball_path))
##     }, error = function(e) {
##         ## is the current latest
##         url <- paste0("https://cran.r-project.org/src/contrib/", names(x), "_", x, ".tar.gz")
##         utils::download.file(url, destfile = tarball_path)
##     })
##     utils::install.packages(pkg = tarball_path, lib = lib, repos = NULL)
##     ## check and error
##     installed_packages <- utils::installed.packages(lib.loc = lib)
##     if (!names(x) %in% dimnames(installed_packages)[[1]]) {
##         stop("Fail to install ", names(x), "\n")
##     }
##     invisible()
## }

.consolidate_sysreqs <- function(granlist) {
    debs <- vapply(strsplit(granlist$deps_sysreqs, "-y "), function(x) x[2], character(1))
    cmd <- paste("apt-get install -y", paste(debs, collapse = " "))
    if ("default-jdk" %in% debs) {
        cmd <- paste(cmd, "liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    }
    paste("apt-get update -qq &&", cmd)
}

#' Export The Resolved Result As Installation Script
#'
#' This function exports the results from [resolve()] to an installation script that can be run in a fresh R environment.
#' @param granlist output from [resolve()]
#' @param path character, path of the exported installation script
#' @return `path`, invisibly
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     export_granlist(graph, "gran.R")
#' }
#' }
export_granlist <- function(granlist, path) {
    install_order <- .determine_installation_order(granlist)
    file.create(path)
    con <- file(path, open="w")
    writeLines(readLines(system.file("header.R", package = "gran")), con = con)
    cat("install_order <- ", file = con)
    dput(install_order, file = con)
    writeLines(readLines(system.file("footer.R", package = "gran")), con = con)
    close(con)
    invisible(path)
}

#' Dockerize The Resolved Result
#'
#' This function exports the result from [resolve()] to a Docker file.
#' @param output_dir where to put the Docker file
#' @param image character, which versioned Rocker image to use. Can only be "r-ver", "rstudio", "tidyverse", "verse", "geospatial"
#' @param ... arguments to be passed to `dockerize`
#' @return `output_dir`, invisibly
#' @inheritParams export_granlist
#' @seealso [resolve()], [export_granlist()]
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     dockerize(graph, ".")
#' }
#' }
#' @export 
dockerize <- function(granlist, output_dir, image = c("r-ver", "rstudio", "tidyverse", "verse", "geospatial")) {
    if (missing(output_dir)) {
        stop("You must provide `output_dir`.")
    }
    if (!grepl("^ubuntu", granlist$os)) {
        stop("System dependencies of ", granlist$os, " can't be dockerized.")
    }
    image <- match.arg(image)
    sysreps_cmd <- .consolidate_sysreqs(granlist)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    gran_path <- file.path(output_dir, "gran.R")
    export_granlist(granlist, gran_path)
    basic_docker <- c("", "", "COPY gran.R ./gran.R", "RUN Rscript gran.R", "CMD [\"R\"]")
    basic_docker[1] <- paste0("FROM rocker/", image, ":", granlist$r_version)
    basic_docker[2] <- paste("RUN", sysreps_cmd)
    if (image == "rstudio") {
        basic_docker[5] <- "EXPOSE 8787"
        basic_docker[6] <- "CMD [\"/init\"]"
    }
    writeLines(basic_docker, file.path(output_dir, "Dockerfile"))
    invisible(output_dir)
}

#' @rdname dockerize
#' @export
dockerize_granlist <- function(...) {
    dockerize(...)
}

#' @rdname dockerize
#' @export
dockerise <- function(...) {
    dockerize(...)
}

#' @rdname dockerize
#' @export
dockerise_granlist <- function(...) {
    dockerize(...)
}
