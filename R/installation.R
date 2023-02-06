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
    if (length(granlist$deps_sysreqs) == 0) {
        return("apt-get update -qq")
    }
    debs <- vapply(strsplit(granlist$deps_sysreqs, "-y "), function(x) x[2], character(1))
    cmd <- paste("apt-get install -y", paste(debs, collapse = " "))
    if ("default-jdk" %in% debs) {
        cmd <- paste(cmd, "liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    }
    paste("apt-get update -qq &&", cmd)
}

.write_granlist_as_comment <- function(granlist, con, path, verbose, lib,
                                       cran_mirror, check_cran_mirror) {
    cat("## ## To reconstruct this file, please install version",
        as.character(utils::packageVersion("gran")), "of `gran` and run:\n", file = con)
    cat("## granlist <- \n", file = con)
    tempgrancontent <- tempfile()
    dput(granlist, file = tempgrancontent)
    granlist_src <- readLines(tempgrancontent)
    writeLines(paste("##", granlist_src), con = con)
    if (is.na(lib)) {
        lib_as_character <- "NA"
    } else {
        lib_as_character <- paste0("\"", lib, "\"")
    }
    writeLines(paste0("## gran::export_granlist(granlist = granlist, path = \"", path, "\", verbose = ",
                     as.character(verbose), ", lib = ", lib_as_character,
                     ", cran_mirror = \"", cran_mirror, "\", check_cran_mirror = ",
                     as.character(check_cran_mirror), ")"), con = con)
}

.check_mirror <- function(mirror) {
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

.cache_r_package <- function(x, cache_dir, cran_mirror, verbose) {
    url <- paste(cran_mirror, "src/contrib/Archive/", names(x), "/", names(x), "_", x, ".tar.gz", sep = "")
    tarball_path <- file.path(cache_dir, paste(names(x), "_", x, ".tar.gz", sep = ""))
    tryCatch({
        suppressWarnings(download.file(url, destfile = tarball_path, quiet = !verbose))
    }, error = function(e) {
        ## is the current latest
        url <- paste(cran_mirror, "src/contrib/", names(x), "_", x, ".tar.gz", sep = "")
        download.file(url, destfile = tarball_path, quiet = !verbose)
    })
    if (!file.exists(tarball_path)) {
        warning(names(x), "(", x,") can't be cache.")
    }
}

.cache_cran <- function(granlist, output_dir, cran_mirror, verbose) {
    install_order <- .determine_installation_order(granlist)
    cache_dir <- file.path(output_dir, "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir)
    }
    for (i in seq_along(install_order)) {
        .cache_r_package(x = install_order[i], cache_dir = cache_dir,
                         cran_mirror = cran_mirror, verbose = verbose)
    }
    ## For #14, cache R source in the future here
    invisible(output_dir)
}

.insert_cache_dir <- function(basic_docker) {
    gran_line <- which(basic_docker == "RUN Rscript gran.R")
    c(basic_docker[1:(gran_line - 1)],
      "COPY cache ./cache",
      basic_docker[gran_line:length(basic_docker)])
}

#' Export The Resolved Result As Installation Script
#'
#' This function exports the results from [resolve()] to an installation script that can be run in a fresh R environment.
#' @param granlist output from [resolve()]
#' @param path character, path of the exported installation script
#' @param granlist_as_comment logical, whether to write granlist and the steps to reproduce
#' the file to `path` as comment
#' @param verbose logical, pass to [install.packages()], the negated value is also passed as `quiet` to both [install.packages()]
#' and [download.file()].
#' @param lib character, pass to [install.packages()]. By default, it is NA (to install the packages to the default location)
#' @param cran_mirror character, which CRAN mirror to use
#' @param check_cran_mirror logical, whether to check the CRAN mirror
#' @return `path`, invisibly
#' @details The idea behind this is to determine the installation order of R packages locally. Then, the installation script can be depolyed to another
#' fresh R session to install R packages. [dockerize()] is a more reasonable way because a fresh R session with all system requirements
#' is provided. The current approach does not work in R < 2.1.0.
#' @export
#' @references
#' Ripley, B. (2005) [Packages and their Management in R 2.1.0.](https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf) R News, 5(1):8--11. 
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     export_granlist(graph, "gran.R")
#' }
#' }
export_granlist <- function(granlist, path, granlist_as_comment = TRUE, verbose = TRUE, lib = NA,
                            cran_mirror = "https://cran.r-project.org/", check_cran_mirror = TRUE) {
    cran_mirror <- .normalize_url(cran_mirror)
    if (isTRUE(check_cran_mirror)) { ## probably need to stop this also if #17 is implemented
        if (isFALSE(.check_mirror(cran_mirror))) {
            stop(cran_mirror, "does not appear to be a valid CRAN mirror.", call. = FALSE)
        }
    }
    if (utils::compareVersion(granlist$r_version, "3.3") == -1) { #20
        cran_mirror <- .normalize_url(cran_mirror, https = FALSE)
    }
    install_order <- .determine_installation_order(granlist)
    file.create(path)
    con <- file(path, open="w")
    writeLines(readLines(system.file("header.R", package = "gran")), con = con)
    cat("install_order <- ", file = con)
    dput(install_order, file = con)
    cat("\n", file = con)
    cat(paste0("verbose <- ", as.character(verbose), "\n"), file = con)
    if (is.na(lib)) {
        cat("lib <- NA\n", file = con)
    } else {
        cat(paste0("lib <- \"", as.character(lib), "\"\n"), file = con)
    }
    cat(paste0("cran_mirror <- \"", cran_mirror, "\"\n"), file = con)    
    writeLines(readLines(system.file("footer.R", package = "gran")), con = con)
    if (isTRUE(granlist_as_comment)) {
        .write_granlist_as_comment(granlist = granlist, con = con, path = path, verbose = verbose,
                                   lib = lib, cran_mirror = cran_mirror,
                                   check_cran_mirror = check_cran_mirror)
    }
    close(con)
    invisible(path)
}

#' Dockerize The Resolved Result
#'
#' This function exports the result from [resolve()] to a Docker file.
#' @param output_dir where to put the Docker file
#' @param image character, which versioned Rocker image to use. Can only be "r-ver", "rstudio", "tidyverse", "verse", "geospatial".
#' This applies only when R version <= 3.1
#' @param cache logical, whether to cache the content from CRAN now. Please note that the system requirements are not cached
#' @param ... arguments to be passed to `dockerize`
#' @return `output_dir`, invisibly
#' @inheritParams export_granlist
#' @inherit export_granlist details references
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
dockerize <- function(granlist, output_dir, image = c("r-ver", "rstudio", "tidyverse", "verse", "geospatial"),
                      granlist_as_comment = TRUE, cache = FALSE, verbose = TRUE, lib = NA,
                      cran_mirror = "https://cran.r-project.org/", check_cran_mirror = TRUE) {
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
    export_granlist(granlist = granlist, path = gran_path, granlist_as_comment = granlist_as_comment,
                    verbose = verbose, lib = lib, cran_mirror = cran_mirror,
                    check_cran_mirror = check_cran_mirror)
    if (isTRUE(cache)) {
        .cache_cran(granlist, output_dir, cran_mirror, verbose)
    }
    basic_docker <- c("", "", "COPY gran.R ./gran.R", "RUN Rscript gran.R", "CMD [\"R\"]")
    if (!is.na(lib)) {
        basic_docker[4] <- paste0("RUN mkdir ", lib, " && Rscript gran.R")
    }
    basic_docker[1] <- paste0("FROM rocker/", image, ":", granlist$r_version)
    basic_docker[2] <- paste("RUN", sysreps_cmd)
    if (image == "rstudio") {
        basic_docker[5] <- "EXPOSE 8787"
        basic_docker[6] <- "CMD [\"/init\"]"
    }
    if (isTRUE(cache)) {
        basic_docker <- .insert_cache_dir(basic_docker)
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
