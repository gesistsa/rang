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

.install_from_cran <- function(x, lib, path = tempdir()) {
    url <- paste0("https://cran.r-project.org/src/contrib/Archive/", names(x), "/", names(x), "_", x, ".tar.gz")
    tarball_path <- file.path(path, paste0(names(x), "_", x, ".tar.gz"))
    tryCatch({
        suppressWarnings(download.file(url, destfile = tarball_path))
    }, error = function(e) {
        ## is the current latest
        url <- paste0("https://cran.r-project.org/src/contrib/", names(x), "_", x, ".tar.gz")
        download.file(url, destfile = tarball_path)
    })
    install.packages(pkg = tarball_path, lib = lib, repos = NULL)
    ## check and error
    installed_packages <- installed.packages(lib.loc = lib)
    if (!names(x) %in% dimnames(installed_packages)[[1]]) {
        stop("Fail to install ", names(x), "\n")
    }
    invisible()
}

.consolidate_sysreqs <- function(granlist) {
    strsplit(granlist$deps_sysreqs, "-y ")
    debs <- vapply(strsplit(granlist$deps_sysreqs, "-y "), function(x) x[2], character(1))
    cmd <- paste("apt-get install -y", paste(debs, collapse = " "))
    if ("default-jdk" %in% debs) {
        cmd <- paste(cmd, "liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    }
    paste("apt-get update -qq &&", cmd)
}

#' Docker The Resolved Result
#'
#' This function exports the result from [resolve()] to a Docker file.
#' @param granlist output from [resolve()]
#' @param output_dir where to put the Docker file
#' @return output_dir, invisibly
#' @examples
#' \donttest{
#' graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                 snapshot_date = "2020-01-16")
#" dockerize(graph, tempdir())
#' }
#' @export 
dockerize <- function(granlist, output_dir) {
    if (missing(output_dir)) {
        stop("You must provide `output_dir`.")
    }
    install_order <- .determine_installation_order(granlist)
    sysreps_cmd <- .consolidate_sysreqs(granlist)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    file.create(file.path(output_dir, "gran.R"))
    con <- file(file.path(output_dir, "gran.R"), open="w")
    writeLines(readLines(system.file("header.R", package = "gran")), con = con)
    cat("install_order <- ", file = con)
    dput(install_order, file = con)
    writeLines(readLines(system.file("footer.R", package = "gran")), con = con)
    close(con)
    basic_docker <- c("", "", "COPY gran.R ./gran.R", "RUN Rscript gran.R", "CMD [\"R\"]")
    basic_docker[1] <- paste0("FROM rocker/r-ver:", granlist$r_version)
    basic_docker[2] <- paste("RUN", sysreps_cmd)
    writeLines(basic_docker, file.path(output_dir, "Dockerfile"))
    invisible(output_dir)
}
