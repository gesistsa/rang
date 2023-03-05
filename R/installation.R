## here, uid is a fastmap; see below
.extract_uid_safe <- function(pkgref, uid) {
    if (is.null(uid$get(pkgref))) {
        return(NA_character_)
    } else {
        return(uid$get(pkgref))
    }
}

.generate_installation_order <- function(rang) {
    dep <- fastmap::fastmap()
    version <- fastmap::fastmap()
    uid <- fastmap::fastmap()
    ## package name as per DESCRIPTION, aka. x
    ## can't use x here because it is too generic
    pkgname <- fastmap::fastmap()
    for (ranglet in rang$ranglets) {
        current_pkgref <- unique(ranglet$original$x_pkgref)
        current_ver <- unique(ranglet$original$x_version)
        current_dep <- .extract_queryable_dependencies(dep_df = ranglet$original, no_enhances = ranglet$no_enhances, no_suggests = ranglet$no_suggests)
        current_pkgname <- unique(ranglet$original$x)
        dep$set(current_pkgref, current_dep)
        version$set(current_pkgref, current_ver)
        pkgname$set(current_pkgref, current_pkgname)
        if ("x_uid" %in% colnames(ranglet$original)) {
            uid$set(current_pkgref, unique(ranglet$original$x_uid))
        }
        for (dep_df in ranglet$deps) {
            current_pkgref <- unique(dep_df$x_pkgref)
            current_ver <- unique(dep_df$x_version)
            current_dep <- .extract_queryable_dependencies(dep_df = dep_df, no_enhances = ranglet$no_enhances, no_suggests = ranglet$no_suggests)
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
    noncranlike_pkgrefs <- c() ## github and local are noncran-like
    needed_pkgrefs <- dep$keys()
    ## install all terminal nodes
    for (pkgref in needed_pkgrefs) {
        if (.parse_pkgref(pkgref, return_handle = FALSE) %in% c("github", "local")) {
            noncranlike_pkgrefs <- c(noncranlike_pkgrefs, pkgref)
            next()
        }
        if (is.null(dep$get(pkgref))) {
            installed_pkgrefs <- c(installed_pkgrefs, pkgref)
        }
    }
    loop_counter <- 0
    while(length(setdiff(needed_pkgrefs, c(installed_pkgrefs, noncranlike_pkgrefs))) != 0) {
        unfulfilled_pkgrefs <- c()
        for (pkgref in needed_pkgrefs) {
            if (!pkgref %in% installed_pkgrefs && !pkgref %in% noncranlike_pkgrefs) {
                ## check requirement
                requirement_fulfilled <- length(setdiff(dep$get(pkgref), installed_pkgrefs)) == 0
                if (requirement_fulfilled) {
                    installed_pkgrefs <- c(installed_pkgrefs, pkgref)
                } else {
                    unfulfilled_pkgrefs <- c(unfulfilled_pkgrefs, pkgref)
                }
            }
        }
        loop_counter <- loop_counter + 1
        if (loop_counter > (length(needed_pkgrefs) * 5)) {
            stop("Can't determine installation order. Please report the to the developers:\n", paste0(unfulfilled_pkgrefs, collapse = ","), call. = FALSE)
        }
    }
    ordered_pkgrefs <- c(installed_pkgrefs, noncranlike_pkgrefs)
    ordered_x <- vapply(ordered_pkgrefs, function(x) pkgname$get(x), character(1), USE.NAMES = FALSE)
    ordered_version <- vapply(ordered_pkgrefs, function(x) version$get(x), character(1), USE.NAMES = FALSE)
    ordered_source <- vapply(ordered_pkgrefs, function(x) .parse_pkgref(x, return_handle = FALSE), character(1), USE.NAMES = FALSE)
    ordered_handle <- vapply(ordered_pkgrefs, function(x) .parse_pkgref(x, return_handle = TRUE), character(1), USE.NAMES = FALSE)
    ordered_uid <- vapply(ordered_pkgrefs, .extract_uid_safe, character(1), uid = uid, USE.NAMES = FALSE)
    data.frame(x = ordered_x, version = ordered_version, source = ordered_source, handle = ordered_handle,
               uid = ordered_uid)
}

.write_rang_as_comment <- function(rang, con, path, verbose, lib,
                                   cran_mirror, check_cran_mirror, bioc_mirror) {
    if (isTRUE(any(grepl("^local::", .extract_pkgrefs(rang))))) {
        cat("## ## WARNING:", file = con)
        cat("## ## Local packages found. The following instructions are not reproducible.", file = con)
    }
    cat("## ## To reconstruct this file, please install version",
        as.character(utils::packageVersion("rang")), "of `rang` and run:\n", file = con)
    cat("## rang <- \n", file = con)
    temprangcontent <- tempfile()
    dput(rang, file = temprangcontent)
    rang_src <- readLines(temprangcontent)
    writeLines(paste("##", rang_src), con = con)
    if (is.na(lib)) {
        lib_as_character <- "NA"
    } else {
        lib_as_character <- paste0("\"", lib, "\"")
    }
    if(!is.null(rang$bioc_version)) {
      bioc_txt <- paste0(", bioc_mirror = \"", bioc_mirror,rang$bioc_version,"/","\"")
    } else {
      bioc_txt <- NULL
    }
    writeLines(paste0("## rang::export_rang(rang = rang, path = \"", path, "\", verbose = ",
                     as.character(verbose), ", lib = ", lib_as_character,
                     ", cran_mirror = \"", cran_mirror, "\", check_cran_mirror = ",
                     as.character(check_cran_mirror), bioc_txt ,")"), con = con)
}

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

.cache_pkgs <- function(rang, output_dir, cran_mirror, bioc_mirror, verbose) {
    installation_order <- .generate_installation_order(rang)
    cache_dir <- file.path(output_dir, "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir)
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
    ## For #14, cache R source in the future here
    invisible(output_dir)
}

.is_r_version_older_than <- function(rang, r_version = "1.3.1") {
    utils::compareVersion(rang$r_version, r_version) == -1
}

.insert_cache_dir <- function(dockerfile_content) {
    rang_line <- which(dockerfile_content == "RUN Rscript rang.R")
    c(dockerfile_content[1:(rang_line - 1)],
      "COPY cache ./cache",
      dockerfile_content[rang_line:length(dockerfile_content)])
}

.insert_materials_dir <- function(dockerfile_content) {
    rang_line <- which(dockerfile_content == "COPY rang.R ./rang.R")
    c(dockerfile_content[1:rang_line],
      "COPY materials/ ./materials/",
      dockerfile_content[(rang_line + 1):length(dockerfile_content)])
}

.generate_debian_eol_dockerfile_content<- function(r_version, lib, sysreqs_cmd, cache, debian_version = "lenny") {
    dockerfile_content <- c(
        paste0("FROM debian/eol:", debian_version),
        "ENV TZ UTC",
        "RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone && apt-get update -qq && apt-get install wget locales build-essential r-base-dev  -y",
        "COPY rang.R ./rang.R",
        "COPY compile_r.sh ./compile_r.sh",
        paste("RUN", sysreqs_cmd),
        paste("RUN bash compile_r.sh", r_version),
        "CMD [\"R\"]")
    if (!is.na(lib)) {
        dockerfile_content[7] <- paste0("RUN mkdir ", lib, " && bash compile_r.sh ", r_version)
    }
    if (isTRUE(cache)) {
        dockerfile_content <- c(dockerfile_content[1:5], "COPY cache ./cache", dockerfile_content[6:8])
    }
    return(dockerfile_content)
}

.generate_rocker_dockerfile_content <- function(r_version, lib, sysreqs_cmd, cache, image) {
    dockerfile_content <- c("", "", "COPY rang.R ./rang.R", "RUN Rscript rang.R", "CMD [\"R\"]")
    if (!is.na(lib)) {
        dockerfile_content[4] <- paste0("RUN mkdir ", lib, " && Rscript rang.R")
    }
    dockerfile_content[1] <- paste0("FROM rocker/", image, ":", r_version)
    dockerfile_content[2] <- paste("RUN", sysreqs_cmd)
    if (image == "rstudio") {
        dockerfile_content[5] <- "EXPOSE 8787"
        dockerfile_content[6] <- "CMD [\"/init\"]"
    }
    if (isTRUE(cache)) {
        dockerfile_content <- .insert_cache_dir(dockerfile_content)
    }
    return(dockerfile_content)
}

.generate_docker_readme <- function(output_dir,image) {
    file.create(file.path(output_dir,"README"))
    con <- file(file.path(output_dir,"README"), open="w")
    readme <- readLines(system.file("readme_template.txt", package = "rang"))
    readme <- gsub("__DATE__",Sys.Date(),readme)
    readme <- gsub("__OUTPUT__",output_dir,readme)
    readme <- gsub("__IMAGE__",image,readme)
    writeLines(readme,file.path(output_dir,"README"))
    close(con)
    invisible(readme)
}

#' Export The Resolved Result As Installation Script
#'
#' This function exports the results from [resolve()] to an installation script that can be run in a fresh R environment.
#' @param rang output from [resolve()]
#' @param path character, path of the exported installation script
#' @param rang_as_comment logical, whether to write resolved result and the steps to reproduce
#' the file to `path` as comment
#' @param verbose logical, pass to [install.packages()], the negated value is also passed as `quiet` to both [install.packages()]
#' and [download.file()].
#' @param lib character, pass to [install.packages()]. By default, it is NA (to install the packages to the default location)
#' @param cran_mirror character, which CRAN mirror to use
#' @param check_cran_mirror logical, whether to check the CRAN mirror
#' @param bioc_mirror character, which Bioconductor mirror to use
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
#'     export_rang(graph, "rang.R")
#' }
#' }
export_rang <- function(rang, path, rang_as_comment = TRUE, verbose = TRUE, lib = NA,
                            cran_mirror = "https://cran.r-project.org/", check_cran_mirror = TRUE,
                            bioc_mirror = "https://bioconductor.org/packages/") {
    if (.is_r_version_older_than(rang, "1.3.1")) {
        stop("`export_rang` doesn't support this R version (yet).")
    }
    if (length(rang$ranglets) == 0) {
        warning("Nothing to export.")
        return(invisible(NULL))
    }
    cran_mirror <- .normalize_url(cran_mirror)
    if (isTRUE(check_cran_mirror)) { ## probably need to stop this also if #17 is implemented
        if (isFALSE(.query_mirror_validity(cran_mirror))) {
            stop(cran_mirror, "does not appear to be a valid CRAN mirror.", call. = FALSE)
        }
    }
    if (.is_r_version_older_than(rang, "3.3")) { #20
        cran_mirror <- .normalize_url(cran_mirror, https = FALSE)
    }
    installation_order <- .generate_installation_order(rang)
    file.create(path)
    con <- file(path, open="w")
    if (.is_r_version_older_than(rang, "2.1")) {
        header_file <- "header_cmd.R"
    } else {
        header_file <- "header.R"
    }
    writeLines(readLines(system.file(header_file, package = "rang")), con = con)
    cat("installation.order <- ", file = con)
    dput(installation_order, file = con)
    cat("\n", file = con)
    cat(paste0("verbose <- ", as.character(verbose), "\n"), file = con)
    if (is.na(lib)) {
        cat("lib <- NA\n", file = con)
    } else {
        cat(paste0("lib <- \"", as.character(lib), "\"\n"), file = con)
    }
    cat(paste0("cran.mirror <- \"", cran_mirror, "\"\n"), file = con)
    if(!is.null(rang$bioc_version)) {
        cat(paste0("bioc.mirror <- \"", "https://bioconductor.org/packages/",rang$bioc_version,"/", "\"\n"), file = con)
    }
    writeLines(readLines(system.file("footer.R", package = "rang")), con = con)
    if (isTRUE(rang_as_comment)) {
        .write_rang_as_comment(rang = rang, con = con, path = path, verbose = verbose,
                                   lib = lib, cran_mirror = cran_mirror,
                                   check_cran_mirror = check_cran_mirror,bioc_mirror = bioc_mirror)
    }
    close(con)
    invisible(path)
}

#' Export The Resolved Result As a renv Lockfile
#'
#' This function exports the results from [resolve()] to a renv lockfile that can be used as an alternative to a docker container.
#' @param rang output from [resolve()]
#' @param path character, path of the exported renv lockfile
#' @return `path`, invisibly
#' @details A renv lockfile is easier to handle than a docker container, but it cannot always reliably reproduce the exact computational environment,especially for very old code.
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     export_renv(graph, ".")
#' }
#' }
export_renv <- function(rang, path = ".") {
    if (length(rang$ranglets) == 0) {
        warning("Nothing to export.")
        return(invisible(NULL))
    }
    pkg_df <- .generate_installation_order(rang)
    pkg_list <- vector(mode = "list",length = nrow(pkg_df))
    names(pkg_list) <- pkg_df$x
    for(i in seq_len(nrow(pkg_df))){
        pkg_list[[i]][["Package"]] <- pkg_df$x[i]
        pkg_list[[i]][["Version"]] <- pkg_df$version[i]
        if(pkg_df$source[i]=="cran"){
            pkg_list[[i]][["Source"]] <- "Repository"
            pkg_list[[i]][["Repository"]] <- "CRAN"
        #   pkg_list[[i]][["Requirements"]] <- c()
        } else if(pkg_df$source[i]=="github"){
            pkg_list[[i]][["Source"]] <- "GitHub"
            pkg_list[[i]][["RemoteType"]] <- "GitHub"
            pkg_list[[i]][["RemoteHost"]] <- "api.github.com"
            pkg_list[[i]][["RemoteRepo"]] <- pkg_df$x[i]
            pkg_list[[i]][["RemoteUsername"]]<- strsplit(pkg_df$handle[i],"/")[[1]][1]
            pkg_list[[i]][["RemoteRef"]] = "HEAD"
            pkg_list[[i]][["RemoteSha"]] = pkg_df$uid[i]
        #   pkg_list[[i]][["Requirements"]] <- c()
        } else if(pkg_df$source[i]=="bioc"){
            pkg_list[[i]][["Source"]] <- "Bioconductor"
            # pkg_list[[i]][["git_url"]] <- paste0("https://git.bioconductor.org/packages/",pkg_df$x[i])
            # pkg_list[[i]][["git_branch"]] <- ""
            # pkg_list[[i]][["git_last_commit"]] <- ""
            # pkg_list[[i]][["git_last_commit_date"]] <- ""
            # pkg_list[[i]][["Requirements"]] <- c()
        } else if(pkg_df$source[i]=="local"){
            pkg_list[[i]][["Source"]] <- "Local"
            pkg_list[[i]][["RemoteType"]] <- "local"
            pkg_list[[i]][["RemoteUrl"]] <- pkg_df$uid[i]
        } else{
          stop("source not supported")
        }
    }
    r_lst <- list(Version = rang$r_version,
                  Repositories = data.frame(Name = "CRAN",URL = "https://cloud.r-project.org"))
    pkg_json <- jsonlite::toJSON(list(R = r_lst,Packages = pkg_list),auto_unbox = TRUE)
    writeLines(jsonlite::prettify(pkg_json), file.path(path,"renv.lock"))
    invisible(pkg_list)
}


#' Dockerize The Resolved Result
#'
#' This function exports the result from [resolve()] to a Docker file. For R version >= 3.1.0, the Dockerfile is based on the versioned Rocker image.
#' For R version < 3.1.0, the Dockerfile is based on Debian and it compiles R from source.
#' @param output_dir character, where to put the Docker file and associated content
#' @param materials_dir character, path to the directory containing additional resources (e.g. analysis scripts) to be copied into `output_dir` and in turn into the Docker container
#' @param image character, which versioned Rocker image to use. Can only be "r-ver", "rstudio", "tidyverse", "verse", "geospatial"
#' This applies only to R version >= 3.1
#' @param cache logical, whether to cache the packages now. Please note that the system requirements are not cached. For query with non-CRAN packages, this option is strongly recommended. For query with local packages, this must be TRUE regardless of R version. For R version < 3.1, this must be also TRUE if there is any non-CRAN packages.
#' @param no_rocker logical, whether to skip using Rocker images even when an appropriate version is available. Please keep this as `TRUE` unless you know what you are doing
#' @param debian_version, when Rocker images are not used, which EOL version of Debian to use. Can only be "lenny", "etch", "squeeze", "wheezy", "jessie", "stretch". Please keep this as default "lenny" unless you know what you are doing
#' @param ... arguments to be passed to `dockerize`
#' @return `output_dir`, invisibly
#' @inheritParams export_rang
#' @inherit export_rang details
#' @seealso [resolve()], [export_rang()]
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
dockerize <- function(rang, output_dir, materials_dir = NULL, image = c("r-ver", "rstudio", "tidyverse", "verse", "geospatial"),
                      rang_as_comment = TRUE, cache = FALSE, verbose = TRUE, lib = NA,
                      cran_mirror = "https://cran.r-project.org/", check_cran_mirror = TRUE,
                      bioc_mirror = "https://bioconductor.org/packages/",
                      no_rocker = FALSE,
                      debian_version = c("lenny", "squeeze", "wheezy", "jessie", "stretch")) {
    if (length(rang$ranglets) == 0) {
        warning("Nothing to dockerize.")
        return(invisible(NULL))
    }
    if (missing(output_dir)) {
        stop("You must provide `output_dir`.", call. = FALSE)
    }
    if (!grepl("^ubuntu", rang$os)) {
        stop("System dependencies of ", rang$os, " can't be dockerized.", call. = FALSE)
    }
    if (.is_r_version_older_than(rang, "1.3.1")) {
        stop("`dockerize` doesn't support this R version (yet):", rang$r_version, call. = FALSE)
    }
    if (!is.null(materials_dir) && !(dir.exists(materials_dir))) {
        stop(paste0("The folder ", materials_dir, " does not exist"), call. = FALSE)
    }
    need_cache <- (isTRUE(any(grepl("^github::", .extract_pkgrefs(rang)))) &&
                   .is_r_version_older_than(rang, "3.1")) ||
        (isTRUE(any(grepl("^bioc::", .extract_pkgrefs(rang)))) &&
         .is_r_version_older_than(rang, "3.3")) ||
        (isTRUE(any(grepl("^local::", .extract_pkgrefs(rang))))) ||
        .is_r_version_older_than(rang, "2.1")
    if (isTRUE(need_cache) && isFALSE(cache)) {
        stop("Packages must be cached. Please set `cache` = TRUE.", call. = FALSE)
    }
    image <- match.arg(image)
    debian_version <- match.arg(debian_version)
    sysreqs_cmd <- .group_sysreqs(rang)
    if (!dir.exists(output_dir)) {
        dir.create(output_dir)
    }
    rang_path <- file.path(output_dir, "rang.R")
    export_rang(rang = rang, path = rang_path, rang_as_comment = rang_as_comment,
                    verbose = verbose, lib = lib, cran_mirror = cran_mirror,
                    check_cran_mirror = check_cran_mirror, bioc_mirror = bioc_mirror)
    if (isTRUE(cache)) {
        .cache_pkgs(rang, output_dir, cran_mirror, bioc_mirror, verbose)
    }
    if (.is_r_version_older_than(rang, "3.1") || isTRUE(no_rocker)) {
        file.copy(system.file("compile_r.sh", package = "rang"), file.path(output_dir, "compile_r.sh"),
                  overwrite = TRUE)

        dockerfile_content <- .generate_debian_eol_dockerfile_content(r_version = rang$r_version,
                                                                      sysreqs_cmd = sysreqs_cmd, lib = lib,
                                                                      cache = cache,
                                                                      debian_version = debian_version)
    } else {
        dockerfile_content <- .generate_rocker_dockerfile_content(r_version = rang$r_version,
                                                                  sysreqs_cmd = sysreqs_cmd, lib = lib,
                                                                  cache = cache, image = image)
    }
    if (!(is.null(materials_dir))) {
        materials_subdir_in_output_dir <- file.path(output_dir, "materials")
        if (isFALSE(dir.exists(materials_subdir_in_output_dir))) {
            dir.create(materials_subdir_in_output_dir)
        }
        file.copy(list.files(materials_dir, full.names = TRUE),
                  materials_subdir_in_output_dir,
                  recursive = TRUE)
        dockerfile_content <- .insert_materials_dir(dockerfile_content)
    }
    writeLines(dockerfile_content, file.path(output_dir, "Dockerfile"))
    .generate_docker_readme(output_dir = output_dir,image = image)
    invisible(output_dir)
}

#' @rdname dockerize
#' @export
dockerize_rang <- function(...) {
    dockerize(...)
}

#' @rdname dockerize
#' @export
dockerise <- function(...) {
    dockerize(...)
}

#' @rdname dockerize
#' @export
dockerise_rang <- function(...) {
    dockerize(...)
}
