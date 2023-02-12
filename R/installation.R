##granlist <- readRDS("tests/testdata/graph.RDS")

.safe_get_uid <- function(pkgref, uid) {
    if (is.null(uid$get(pkgref))) {
        return(NA_character_)
    } else {
        return(uid$get(pkgref))
    }
}

.determine_installation_order <- function(granlist) {
    dep <- fastmap::fastmap()
    version <- fastmap::fastmap()
    uid <- fastmap::fastmap()
    ## package name as per DESCRIPTION, aka. x
    ## can't use x here because it is too generic
    pkgname <- fastmap::fastmap()
    for (gran in granlist$grans) {
        current_pkgref <- unique(gran$original$x_pkgref)
        current_ver <- unique(gran$original$x_version)
        current_dep <- .keep_queryable_dependencies(dep_df = gran$original, no_enhances = gran$no_enhances, no_suggests = gran$no_suggests)
        current_pkgname <- unique(gran$original$x)
        dep$set(current_pkgref, current_dep)
        version$set(current_pkgref, current_ver)
        pkgname$set(current_pkgref, current_pkgname)
        if ("x_uid" %in% colnames(gran$original)) {
            uid$set(current_pkgref, unique(gran$original$x_uid))
        }
        for (dep_df in gran$deps) {
            current_pkgref <- unique(dep_df$x_pkgref)
            current_ver <- unique(dep_df$x_version)
            current_dep <- .keep_queryable_dependencies(dep_df = dep_df, no_enhances = gran$no_enhances, no_suggests = gran$no_suggests)
            current_pkgname <- unique(dep_df$x)
            dep$set(current_pkgref, current_dep)
            version$set(current_pkgref, current_ver)
            pkgname$set(current_pkgref, current_pkgname)
            if ("x_uid" %in% colnames(dep_df)) { ## Not supported, but no harm to add it now
                uid$set(current_pkgref, unique(dep_df$x_uid))
            }   
        }
    }
    ## installation simulation
    installed_pkgrefs <- c()
    noncran_pkgrefs <- c()
    needed_pkgrefs <- dep$keys()
    ## install all terminal nodes
    for (pkgref in needed_pkgrefs) {
        if(.is_github(pkgref)){
          noncran_pkgrefs <- c(noncran_pkgrefs, pkgref)
          next()
        }
        if (is.null(dep$get(pkgref))) {
            installed_pkgrefs <- c(installed_pkgrefs, pkgref)
        }
    }
    while(length(setdiff(needed_pkgrefs, c(installed_pkgrefs, noncran_pkgrefs))) != 0) {
        for (pkgref in needed_pkgrefs) {
            ##print(package)
            if (!pkgref %in% installed_pkgrefs & !pkgref %in% noncran_pkgrefs) {
                ## check requirement
                requirement_fulfilled <- length(setdiff(dep$get(pkgref), installed_pkgrefs)) == 0
                if (requirement_fulfilled) {
                    installed_pkgrefs <- c(installed_pkgrefs, pkgref)
                }
            }
        }
    }
    ordered_pkgrefs <- c(installed_pkgrefs, noncran_pkgrefs)
    ordered_x <- vapply(ordered_pkgrefs, function(x) pkgname$get(x), character(1), USE.NAMES = FALSE)
    ordered_version <- vapply(ordered_pkgrefs, function(x) version$get(x), character(1), USE.NAMES = FALSE)
    ordered_source <- vapply(ordered_pkgrefs, function(x) .parse_pkgref(x, return_handle = FALSE), character(1), USE.NAMES = FALSE)
    ordered_handle <- vapply(ordered_pkgrefs, function(x) .parse_pkgref(x, return_handle = TRUE), character(1), USE.NAMES = FALSE)
    ordered_uid <- vapply(ordered_pkgrefs, .safe_get_uid, character(1), uid = uid, USE.NAMES = FALSE)
    data.frame(x = ordered_x, version = ordered_version, source = ordered_source, handle = ordered_handle,
               uid = ordered_uid)
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

.has_ppa_in_sysreqs <- function(granlist, warn = TRUE) {
    res <- isTRUE(any(grepl("add-apt-repository", granlist$deps_sysreqs)))
    if (isTRUE(res) && isTRUE(warn)) {
        warning("The command for getting system requirements is likely not going to work for the default Docker images. You might need to requery system requirements with another version of Ubuntu.", call. = FALSE)
    }
    return(res)
}

.group_apt_cmds <- function(cmds, fix_libgit2 = FALSE) {
    debs <- vapply(strsplit(cmds, "-y "), function(x) x[2], character(1))
    if (isTRUE(fix_libgit2) && "libgit2-dev" %in% debs) {
        if ("libcurl4-openssl-dev" %in% debs) {
            debs[debs == "libcurl4-openssl-dev"] <- "libcurl4-gnutls-dev"
        }
    }
    cmd <- paste("apt-get install -y", paste(debs, collapse = " "))
    if ("default-jdk" %in% debs) {
        cmd <- paste(cmd, "liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    }
    return(cmd)
}

.consolidate_sysreqs <- function(granlist) {
    if (length(granlist$deps_sysreqs) == 0) {
        return("apt-get update -qq")
    }
    if (isFALSE(.has_ppa_in_sysreqs(granlist))) {
        cmds <- granlist$deps_sysreqs
        prefix <- ""
        cmd <- .group_apt_cmds(cmds, fix_libgit2 = TRUE)
    } else {
        update_index <- which("apt-get update" == granlist$deps_sysreqs)
        cmds <- granlist$deps_sysreqs[(update_index + 1):length(granlist$deps_sysreqs)]
        prefix <- paste0(paste(granlist$deps_sysreqs[1:update_index], collapse = " && "), " && ")
        cmd <- .group_apt_cmds(cmds, fix_libgit2 = FALSE)
    }
    paste0("apt-get update -qq && ", prefix, cmd)
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

.cache_cran <- function(x, version, cache_dir, cran_mirror, verbose) {
    url <- paste(cran_mirror, "src/contrib/Archive/", x, "/", x, "_", version, ".tar.gz", sep = "")
    tarball_path <- file.path(cache_dir, paste(x, "_", version, ".tar.gz", sep = ""))
    tryCatch({
        suppressWarnings(utils::download.file(url, destfile = tarball_path, quiet = !verbose))
    }, error = function(e) {
        ## is the current latest
        url <- paste(cran_mirror, "src/contrib/", x, "_", version, ".tar.gz", sep = "")
        utils::download.file(url, destfile = tarball_path, quiet = !verbose)
    })
    if (!file.exists(tarball_path)) {
        warning(names(x), "(", x,") can't be cache.")
    }
}

.gen_temp_dir <- function() {
    file.path(tempdir(), paste(sample(c(LETTERS, letters), 20, replace = TRUE), collapse = ""))
}

.cache_github <- function(x, version, handle, source, uid, cache_dir, verbose) {
    sha <- uid
    short_sha <- substr(sha, 1, 7)
    dest_zip <- tempfile(fileext = ".zip")
    tmp_dir <- .gen_temp_dir()
    ## unlike inside the container, we use zip here because it is less buggy
    download.file(paste("https://api.github.com/repos/", handle, "/zipball/", sha, sep = ""), destfile = dest_zip,
                  quiet = !verbose)
    unzip(dest_zip, exdir = tmp_dir)
    dlist <- list.dirs(path = tmp_dir, recursive = FALSE)
    pkg_dir <- dlist[grepl(short_sha, dlist)]
    if (length(pkg_dir) != 1) {
        stop(paste0("couldn't uniquely locate the unzipped package source in ", tmp_dir))
    }
    res <- system(command = paste("R", "CMD", "build", pkg_dir), intern = TRUE)
    expected_tarball_path <- paste(x, "_", version, ".tar.gz", sep = "")
    if (!file.exists(expected_tarball_path)) {
        stop("Cannot locate the built tarball.")
    }
    file.rename(from = expected_tarball_path, to = file.path(cache_dir, expected_tarball_path))
    unlink(expected_tarball_path)
}


.cache_pkgs <- function(granlist, output_dir, cran_mirror, verbose) {
    install_order <- .determine_installation_order(granlist)
    cache_dir <- file.path(output_dir, "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir)
    }
    for (i in seq(from = 1, to = nrow(install_order), by = 1)) {
        x <- install_order$x[i]
        source <- install_order$source[i]
        version <- install_order$version[i]
        handle <- install_order$handle[i]
        uid <- install_order$uid[i]
        if (source == "cran") {
            .cache_cran(x = x, version = version, cache_dir = cache_dir,
                        cran_mirror = cran_mirror, verbose = verbose)
        }
        if (source == "github") {
            .cache_github(x = x, version = version, handle = handle,
                          source = source, uid = uid,
                          cache_dir = cache_dir, verbose = verbose)
        }
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

.insert_materials_dir <- function(basic_docker){
  gran_line <- which(basic_docker == "COPY gran.R ./gran.R")
  c(basic_docker[1:gran_line], 
    "COPY materials/ ./materials/",
    basic_docker[(gran_line + 1):length(basic_docker)])
}

.generate_pre310_docker <- function(r_version, debian_version = "lenny", lib, sysreqs_cmd, cache) {
    basic_docker <- c(
        paste0("FROM debian/eol:", debian_version),
        "ENV TZ UTC",
        "RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev  -y",
        "COPY gran.R ./gran.R",
        "COPY compile_r.sh ./compile_r.sh",
        paste("RUN", sysreqs_cmd),
        paste("RUN bash compile_r.sh", r_version),
        "CMD [\"R\"]")
    if (!is.na(lib)) {
        basic_docker[7] <- paste0("RUN mkdir ", lib, " && bash compile_r.sh ", r_version)
    }
    if (isTRUE(cache)) {
        basic_docker <- c(basic_docker[1:5], "COPY cache ./cache", basic_docker[6:8])
    }
    return(basic_docker)
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
    if (utils::compareVersion(granlist$r_version, "2.1") == -1) {
        stop("`export_granlist` doesn't support this R version (yet).")
    }
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
#' This function exports the result from [resolve()] to a Docker file. For R version >= 3.1.0, the Dockerfile is based on the versioned Rocker image.
#' For R version < 3.1.0, the Dockerfile is based on Debian and it compiles R from source.
#' @param output_dir character, where to put the Docker file and associated content
#' @param materials_dir character, path to the directiry containing dditional resources (e.g. analysis scripts) to be copied into `output_dir` and in turn into the Docker container
#' @param image character, which versioned Rocker image to use. Can only be "r-ver", "rstudio", "tidyverse", "verse", "geospatial"
#' This applies only to R version <= 3.1
#' @param cache logical, whether to cache the content from CRAN now. Please note that the system requirements are not cached
#' @param ... arguments to be passed to `dockerize`
#' @return `output_dir`, invisibly
#' @inheritParams export_granlist
#' @inherit export_granlist details
#' @seealso [resolve()], [export_granlist()]
#' @references
#' [The Rocker Project](https://rocker-project.org)
#' Ripley, B. (2005) [Packages and their Management in R 2.1.0.](https://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf) R News, 5(1):8--11.
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     dockerize(graph, ".")
#' }
#' }
#' @export
dockerize <- function(granlist, output_dir, materials_dir = NULL, image = c("r-ver", "rstudio", "tidyverse", "verse", "geospatial"),
                      granlist_as_comment = TRUE, cache = FALSE, verbose = TRUE, lib = NA,
                      cran_mirror = "https://cran.r-project.org/", check_cran_mirror = TRUE) {
    if (missing(output_dir)) {
        stop("You must provide `output_dir`.", call. = FALSE)
    }
    if (!grepl("^ubuntu", granlist$os)) {
        stop("System dependencies of ", granlist$os, " can't be dockerized.", call. = FALSE)
    }
    if (utils::compareVersion(granlist$r_version, "2.1") == -1) {
        stop("`dockerize` doesn't support this R version (yet):", granlist$r_version, call. = FALSE)
    }
    if (!is.null(materials_dir) && !(dir.exists(materials_dir))) {
        stop(paste0("The folder ", materials_dir, " does not exist"), call. = FALSE)
    }
    image <- match.arg(image)
    sysreqs_cmd <- .consolidate_sysreqs(granlist)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    gran_path <- file.path(output_dir, "gran.R")
    export_granlist(granlist = granlist, path = gran_path, granlist_as_comment = granlist_as_comment,
                    verbose = verbose, lib = lib, cran_mirror = cran_mirror,
                    check_cran_mirror = check_cran_mirror)
    if (isTRUE(cache)) {
        .cache_pkgs(granlist, output_dir, cran_mirror, verbose)
    }
    if (utils::compareVersion(granlist$r_version, "3.1") == -1) {
        file.copy(system.file("compile_r.sh", package = "gran"), file.path(output_dir, "compile_r.sh"),
                  overwrite = TRUE)
        basic_docker <- .generate_pre310_docker(materials_dir, r_version = granlist$r_version,
                                                sysreqs_cmd = sysreqs_cmd, lib = lib,
                                                cache = cache)
    } else {
        basic_docker <- c("", "", "COPY gran.R ./gran.R", "RUN Rscript gran.R", "CMD [\"R\"]")
        if (!is.na(lib)) {
            basic_docker[4] <- paste0("RUN mkdir ", lib, " && Rscript gran.R")
        }
        basic_docker[1] <- paste0("FROM rocker/", image, ":", granlist$r_version)
        basic_docker[2] <- paste("RUN", sysreqs_cmd)
        if (image == "rstudio") {
            basic_docker[5] <- "EXPOSE 8787"
            basic_docker[6] <- "CMD [\"/init\"]"
        }
        if (isTRUE(cache)) {
            basic_docker <- .insert_cache_dir(basic_docker)
        }
    }
    if (!(is.null(materials_dir))) {
        out_mat_dir <- file.path(output_dir, "materials")
        if (isFALSE(dir.exists(out_mat_dir))) {
            dir.create(out_mat_dir)
        }
        file.copy(list.files(materials_dir, full.names = TRUE), 
                  out_mat_dir, 
                  recursive = TRUE)
        basic_docker <- .insert_materials_dir(basic_docker)
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
