#' Query for System Requirements
#'
#' This function takes an S3 object returned from [resolve()] and (re)queries the System Requirements.
#' @inheritParams export_rang
#' @inheritParams resolve
#' @inherit resolve return
#' @export
#' @seealso [resolve()]
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                 snapshot_date = "2020-01-16", query_sysreqs = FALSE)
#'     graph$sysreqs
#'     graph2 <- query_sysreqs(graph, os = "ubuntu-20.04")
#'     graph2$sysreqs
#' }
#' }
query_sysreqs <- function(rang, os = "ubuntu-20.04") {
    rang$os <- os
    rang$sysreqs <- .query_sysreqs(rang = rang, os = os)
    return(rang)
}

.query_sysreqs <- function(rang, os = "ubuntu-20.04") {
    pkgrefs <- .extract_pkgrefs(rang)
    if (length(pkgrefs) == 0) {
        warning("No packages to query for system requirements.", call. = FALSE)
        return(NA)
    }
    tryCatch({
        return(.query_sysreqs_smart(pkgrefs = pkgrefs, os = os))
    }, error = function(e) {
        return(.query_sysreqs_safe(pkgrefs = pkgrefs, os = os))
    })
}

.query_sysreqs_smart <- function(pkgrefs, os = "ubuntu-20.04") {
    output <- list()
    grouped_handles <- .group_pkgrefs_by_source(pkgrefs)
    if ("github" %in% names(grouped_handles)) {
        output[["github"]] <- .query_sysreqs_github(grouped_handles[["github"]], os = os)
    }
    if ("cran" %in% names(grouped_handles)) {
        output[["cran"]] <- .query_sysreqs_cran(grouped_handles[["cran"]], os = os)
    }
    if ("bioc" %in% names(grouped_handles)) {
        output[["bioc"]] <- .query_sysreqs_bioc(grouped_handles[["bioc"]], os = os)
    }
    if ("local" %in% names(grouped_handles)) {
        output[["local"]] <- .query_sysreqs_local(grouped_handles[["local"]], os = os)
    }
    unique(unlist(output))
}

.query_sysreqs_safe <- function(pkgrefs, os = "ubuntu-20.04") {
    output <- c()
    for (pkgref in pkgrefs) {
        source <- .parse_pkgref(pkgref, FALSE)
        switch(source,
               "cran" = {
                   query_fun <- .query_sysreqs_cran
               },
               "github" = {
                   query_fun <- .query_sysreqs_github
               },
               "bioc" = {
                   query_fun <- .query_sysreqs_bioc
               })
        tryCatch({
            result <- query_fun(handles = .parse_pkgref(pkgref), os = os)
            output <- c(output, result)
        }, error = function(e) {
            warning(pkgref, " can't be queried for System requirements. Assumed to have no requirement.", call. = FALSE)
        })
    }
    return(unique(output))
}

## this is vectorized; and for consistency
.query_sysreqs_cran <- function(handles, os) {
    remotes::system_requirements(package = handles, os = os)
}

.query_sysreqs_github <- function(handles, os) {
    res <- lapply(handles, .query_sysreqs_github_single, os = os)
    unique(unlist(res))
}

.query_sysreqs_bioc <- function(handles, os) {
    pkgs <- .memo_search_bioc(bioc_version = "release")
    raw_sys_reqs <- pkgs$SystemRequirements[pkgs$Package %in% handles]
    singleline_sysreqs <- paste0(raw_sys_reqs[!is.na(raw_sys_reqs)], collapse = ", ")
    singleline_sysreqs <- gsub("\\n", " ", singleline_sysreqs)
    .query_singleline_sysreqs(singleline_sysreqs = singleline_sysreqs, os = os)
}

.query_sysreqs_local <- function(handles, os) {
    description_paths <- vapply(handles, .extract_local_description_path, FUN.VALUE = character(1))
    raw_sys_reqs <- vapply(description_paths, FUN = function(x) read.dcf(x, fields = "SystemRequirements")[,1],
                           FUN.VALUE = character(1))
    singleline_sysreqs <- paste0(raw_sys_reqs[!is.na(raw_sys_reqs)], collapse = ", ")
    singleline_sysreqs <- gsub("\\n", " ", singleline_sysreqs)
    .query_singleline_sysreqs(singleline_sysreqs = singleline_sysreqs, os = os)
}

.query_singleline_sysreqs <- function(singleline_sysreqs, os = "ubuntu-20.04") {
    if (grepl("^ubuntu|^debian", os)) {
        arch <- "DEB"
    }
    if (grepl("^centos|^fedora|^redhat", os)) {
        arch <- "RPM"
    }
    baseurl <- "https://sysreqs.r-hub.io/map/"
    url <- utils::URLencode(paste0(baseurl, singleline_sysreqs))
    query_res <- httr::content(httr::GET(url))
    checkable_cmds <- vapply(query_res, .extract_sys_package, character(1), arch = arch)
    uncheckable_cmds <- .extract_uncheckable_sysreqs(singleline_sysreqs, arch = arch)
    c(checkable_cmds[!is.na(checkable_cmds)], uncheckable_cmds)
}

## Not everything can be check from sysreqs DB, especially Bioc packages
## https://github.com/r-hub/sysreqsdb
.extract_uncheckable_sysreqs <- function(singleline_sysreqs, arch) {
    uncheckable_sysreqs <- list(liblzma = c("DEB" = "liblzma-dev", "RPM" = "xz-devel"),
                                libbz2 = c("DEB" = "libbz2-dev", "RPM" = "libbz2-devel"),
                                `Tcl/Tk` = c("DEB" = "tcl8.6 tk8.6", "RPM" = "tcl tk"))
    cmds <- c()
    prefix <- c("DEB" = "apt-get install -y", "RPM" = "dnf install -y")
    for (regex in names(uncheckable_sysreqs)) {
        if (grepl(regex, singleline_sysreqs)) {
            cmds <- c(cmds, paste(prefix[arch], uncheckable_sysreqs[[regex]][arch]))
        }
    }
    return(cmds)
}

.extract_item_cheat <- function(item, arch) {
    cheat_code <- list("libgsl" = c("DEB" = "libgsl0-dev", "RPM" = "libgsl-devel"),
                       "mysql-client" = c("DEB" = "libmysqlclient-dev", "RPM" = "mariadb-devel"))
    for (libname in names(cheat_code)) {
        if (names(item) == libname) {
            return(cheat_code[[libname]][arch])
        }
    }
    return(NULL)
}

.extract_sys_package <- function(item, arch = "DEB") {
    output <- item[[names(item)]]$platforms[[arch]]
    if (isFALSE(is.list((output)))) {
        sys_pkg <- output
    } else {
        sys_pkg <- output[["buildtime"]]
    }
    ## VW style cheating for libgsl
    ## if (names(item) == "libgsl" && arch == "DEB") {
    ##     ## libgsl0-dev is a virtual package of libgsl-dev in later distros anyway
    ##     sys_pkg <- "libgsl0-dev"
    ## }
    ## if (names(item) == "libgsl" && arch == "RPM") {
    ##     sys_pkg <- "libgsl-devel"
    ## }
    if (is.null(sys_pkg)) {
        sys_pkg <- .extract_item_cheat(item, arch)
    }
    if (is.null(sys_pkg)) {
        return(NA_character_)
    }
    if (arch == "DEB") {
        return(paste0("apt-get install -y ", sys_pkg))
    }
    if (arch == "RPM") {
        return(paste0("dnf install -y ", sys_pkg))
    }
}

## .query_sysreqs_posit <- function(description_file, os, remove_description = TRUE) {
##     os_info <- strsplit(os, "-")[[1]]
##     DEFAULT_RSPM <- "https://packagemanager.rstudio.com"
##     DEFAULT_RSPM_REPO_ID <- "1"
##     curl <- Sys.which("curl")
##     rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
##     rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)
##     rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)
##     res <- system2(
##         curl,
##         args = c(
##             "--silent",
##             "--data-binary",
##             shQuote(paste0("@", description_file)),
##             shQuote(sprintf("%s/sysreqs?distribution=%s&release=%s&suggests=false",
##                             rspm_repo_url,
##                             os_info[1],
##                             os_info[2])
##                     )
##         ),
##         stdout = TRUE
##     )
##     res <- jsonlite::fromJSON(res,simplifyDataFrame = FALSE)
##     if (!is.null(res$error)) {
##         stop(res$error)
##     }
##     if (isTRUE(remove_description)) {
##         file.remove(description_file)
##     }
##     unique(unlist(c(res[["install_scripts"]],
##                     lapply(res[["dependencies"]], `[[`, "install_scripts"))))
## }

## get system requirements for github packages
.query_sysreqs_github_single <- function(handle, os) {
    description_file <- tempfile()
    ## potential issue: not going back to snapshot time! but the same is true for the remotes approach?
    repo_descr <- .gh(paste0("/repos/", handle, "/contents/DESCRIPTION"))
    con <- url(repo_descr$download_url)
    singleline_sysreqs <- read.dcf(con, "SystemRequirements")[1,1]
    close(con)
    if (is.null(singleline_sysreqs)) {
        return(NULL)
    }
    .query_singleline_sysreqs(singleline_sysreqs, os)
    ##writeLines(readLines(repo_descr$download_url), con = description_file)
    ##.query_sysreqs_posit(description_file = description_file, os = os, remove_description = TRUE)
}

.is_ppa_in_sysreqs <- function(rang, warn = TRUE) {
    res <- isTRUE(any(grepl("add-apt-repository", rang$sysreqs)))
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
    cmd <- paste("apt-get install -y", paste(sort(debs), collapse = " "))
    if ("default-jdk" %in% debs) {
        cmd <- paste(cmd, "liblzma-dev libpcre3-dev libbz2-dev && R CMD javareconf")
    }
    return(cmd)
}

.group_sysreqs <- function(rang) {
    must_do_cmd <- "apt-get update -qq && apt-get install -y libpcre3-dev zlib1g-dev pkg-config"
    if (length(rang$sysreqs) == 0) {
        must_do_cmd <- paste(must_do_cmd, "libcurl4-openssl-dev")
        return(must_do_cmd)
    }
    if (isFALSE(.is_ppa_in_sysreqs(rang))) {
        cmds <- rang$sysreqs
        prefix <- ""
        cmd <- .group_apt_cmds(cmds, fix_libgit2 = TRUE)
        if (!grepl("libcurl4-gnutls-dev", cmd)) {
            must_do_cmd <- paste(must_do_cmd, "libcurl4-openssl-dev")
        }
    } else {
        cmds <- setdiff(rang$sysreqs, c("apt-get install -y software-properties-common", "apt-get update"))
        ppa_lines <- c("apt-get install -y software-properties-common",
                       grep("^add-apt-repository", rang$sysreqs, value = TRUE),
                       "apt-get update")
        cmds <- setdiff(rang$sysreqs, ppa_lines)
        prefix <- paste0(paste0(ppa_lines, collapse = " && "), " && ")
        cmd <- .group_apt_cmds(cmds, fix_libgit2 = FALSE)
    }
    paste0(must_do_cmd, " && ", prefix, cmd)
}
