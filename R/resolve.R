.query_rver <- function(snapshot_date) {
    if (snapshot_date < attr(cached_rver, "newest_date")) {
        allvers <- cached_rver
    } else {
        allvers <- .memo_rver()
    }
    allvers$date <- parsedate::parse_date(allvers$date)
    utils::tail(allvers[allvers$date < snapshot_date,], 1)$version
}

.query_biocver <- function(snapshot_date) {
    if (snapshot_date < attr(cached_biocver, "newest_date")) {
        allvers <- cached_biocver
    } else {
        allvers <- .memo_biocver()
    }
    allvers$date <- parsedate::parse_date(allvers$date)
    utils::tail(allvers[allvers$date < snapshot_date,], 1)[,1:2]
}

## get the latest version as of date
## let's call this output dep_df; basically is a rough version of edgelist
.query_snapshot_dependencies <- function(pkgref = "cran::rtoot", snapshot_date = "2022-12-10", bioc_version) {
    source <- .parse_pkgref(pkgref, return_handle = FALSE)
    switch(source,
           "cran" = {
               return(.query_snapshot_dependencies_cran(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date,
                                                        bioc_version = bioc_version))
           },
           "github" = {
               return(.query_snapshot_dependencies_github(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date,
                                                          bioc_version = bioc_version))
           },
           "bioc" = {
               ## no need to have bioc_version because it will get queried once again
               return(.query_snapshot_dependencies_bioc(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date))
           },
           "local" = {
               return(.query_snapshot_dependencies_local(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date,
                                                          bioc_version = bioc_version))
           })
}

.query_snapshot_dependencies_cran <- function(handle = "rtoot", snapshot_date = "2022-12-10", bioc_version = NULL) {
    snapshot_date <- parsedate::parse_date(snapshot_date)
    search_res <- .memo_search(handle)
    search_res$pubdate <- parsedate::parse_date(search_res$crandb_file_date)
    snapshot_versions <- search_res[search_res$pubdate <= snapshot_date,]
    if (nrow(snapshot_versions) == 0) {
        stop("No snapshot version exists for ", handle, ".",  call. = FALSE)
    }
    latest_version <- utils::tail(snapshot_versions[order(snapshot_versions$pubdate),], n = 1)
    dependencies <- latest_version$dependencies[[1]]
    if (nrow(dependencies) == 0) {
        return(data.frame(snapshot_date = snapshot_date, x = handle, x_version = latest_version$Version,
                          x_pubdate = latest_version$pubdate, x_pkgref = .normalize_pkgs(handle, bioc_version = bioc_version)))
    }
    data.frame(snapshot_date = snapshot_date, x = handle,
               x_version = latest_version$Version,
               x_pubdate = latest_version$pubdate,
               x_pkgref = .normalize_pkgs(handle, bioc_version = bioc_version),
               y = dependencies$package, type = dependencies$type,
               y_raw_version = dependencies$version,
               y_pkgref = .normalize_pkgs(dependencies$package, bioc_version = bioc_version))
}

.query_snapshot_dependencies_github <- function(handle = "schochastics/rtoot", snapshot_date = "2022-12-10", bioc_version = NULL) {
    snapshot_date <- parsedate::parse_date(snapshot_date)
    sha <- .query_sha(handle, snapshot_date)
    repo_descr <- .gh(paste0("/repos/", handle,"/contents/DESCRIPTION"), ref = sha$sha)
    con <- url(repo_descr$download_url)
    descr_df <- as.data.frame(read.dcf(con))
    close(con)
    pkg_dep_df <- .parse_desc(descr_df,snapshot_date)
    pkg_dep_df$x_pkgref <- .normalize_pkgs(pkgs = handle, bioc_version = bioc_version)
    pkg_dep_df$x_uid <- sha$sha
    pkg_dep_df$x_pubdate <- sha$x_pubdate
    if (isFALSE("y" %in% names(pkg_dep_df))) {
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid")])
    }
    pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y, bioc_version = bioc_version)
    pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid", "y", "type", "y_raw_version", "y_pkgref")]
}

.query_snapshot_dependencies_bioc <- function(handle = "BiocGenerics", snapshot_date = "2022-01-10") {
    snapshot_date <- parsedate::parse_date(snapshot_date)
    bioc_version_df <- .query_biocver(snapshot_date) ## a dataframe!
    search_res <- .memo_search_bioc(bioc_version_df$version)
    search_res$pubdate <- parsedate::parse_date(bioc_version_df$date)
    latest_version <- search_res[search_res$Package==handle,]
    if (nrow(latest_version) == 0) {
        stop("No snapshot version exists for ", handle, ".",  call. = FALSE)
    }
    pkg_dep_df <- .parse_desc(descr_df = latest_version,snapshot_date = snapshot_date)
    pkg_dep_df$x_bioc_ver <- bioc_version_df$version
    if ("y" %in% colnames(pkg_dep_df)) {
        pkg_dep_df <- pkg_dep_df[!is.na(pkg_dep_df$y),]
    }
    pkg_dep_df$x_pubdate <- bioc_version_df$date
    pkg_dep_df$x_pkgref <- .normalize_pkgs(handle, bioc_version = bioc_version_df$version)
    pkg_dep_df$x_uid <- latest_version$suffix
    if (isFALSE("y" %in% names(pkg_dep_df))) {
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_bioc_ver", "x_uid")])
    }
    pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y, bioc_version = bioc_version_df$version)
    pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_bioc_ver", "x_uid", "y", "type", "y_raw_version", "y_pkgref")]
}

.read_tarball_description <- function(path) {
    tempfile <- tempfile()
    DESCRIPTION_in_tarball <- grep("DESCRIPTION$", utils::untar(path, list = TRUE), value = TRUE)
    utils::untar(path, files = DESCRIPTION_in_tarball, exdir = tempfile)
    list.files(tempfile, pattern = "DESCRIPTION", full.names = TRUE, recursive = TRUE)[1]
}

.extract_local_description_path <- function(handle) {
    ## .check_local_in_pkgrefs did the check already
    if (.is_directory(handle)) {
        return(file.path(handle, "DESCRIPTION"))
    }
    .read_tarball_description(handle)
}

.query_snapshot_dependencies_local <- function(handle, snapshot_date, bioc_version = NULL) {
    snapshot_date <- parsedate::parse_date(snapshot_date)
    description_path <- .extract_local_description_path(handle)
    descr_df <- as.data.frame(read.dcf(description_path))
    pkg_dep_df <- .parse_desc(descr_df, snapshot_date)
    pkg_dep_df$x_pkgref <- .normalize_pkgs(pkgs = handle, bioc_version = bioc_version)
    pkg_dep_df$x_uid <- normalizePath(handle)
    pkg_dep_df$x_pubdate <- snapshot_date
    if (isFALSE("y" %in% names(pkg_dep_df))) {
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid")])
    }
    pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y, bioc_version = bioc_version)
    pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid", "y", "type", "y_raw_version", "y_pkgref")]
}

## get the commit sha for the commit closest to date
.query_sha <- function(handle, date) {
    commits <- .gh(paste0("/repos/", handle, "/commits"), per_page = 100)
    dates <- sapply(commits,function(x) x$commit$committer$date)
    idx <- which(dates<=date)[1]
    k <- 2
    while(is.null(idx)) {
        commits <- .gh(paste0("/repos/", handle, "/commits"), per_page = 100, page = k)
        k <- k + 1
    }
    list(sha = commits[[idx]]$sha, x_pubdate =  parsedate::parse_date(dates[[idx]]))
}

# parse a description file from github repo
.parse_desc <- function(descr_df, snapshot_date = "2019-08-31", remotes = FALSE) {
    types <- c("Depends","LinkingTo","Imports","Suggests","Enhances")
    depends <- descr_df[["Depends"]]
    imports <- descr_df[["Imports"]]
    linking <- descr_df[["LinkingTo"]]
    suggests <- descr_df[["Suggests"]]
    enhances <- descr_df[["Enhances"]]
    if(!is.null(imports))  imports <- trimws(strsplit(imports, ",[\n]*")[[1]])
    if(!is.null(linking))  linking <- trimws(strsplit(linking, ",[\n]*")[[1]])
    if(!is.null(suggests)) suggests <- trimws(strsplit(suggests, ",[\n]*")[[1]])
    if(!is.null(enhances)) enhances <- trimws(strsplit(enhances, ",[\n]*")[[1]])
    if(!is.null(depends))  depends <- trimws(strsplit(depends, ",[\n]*")[[1]])
    if (isFALSE(remotes)) {
        raw_deps <- list(depends, linking, imports, suggests, enhances)
    } else {
        types <- c(types, "Remotes")
        remotes <- descr_df[["Remotes"]]
        if(!is.null(remotes))  remotes <- trimws(strsplit(remotes, ",[\n]*")[[1]])
        raw_deps <- list(depends, linking, imports, suggests, enhances, remotes)
    }
    type <- lapply(seq_along(raw_deps), function(x) rep(types[x], length(raw_deps[[x]])))
    version <- vapply(unlist(raw_deps), .extract_version, character(1), USE.NAMES = FALSE)
    deps <- gsub("\\s*\\(.*\\)","",unlist(raw_deps))
    if (length(deps) == 0 || all(is.na(deps))) {
        return(data.frame(
            snapshot_date = as.Date(snapshot_date),
            x = descr_df[["Package"]],
            x_version = descr_df[["Version"]]
        ))
    }
    data.frame(
        snapshot_date = snapshot_date,
        x = descr_df[["Package"]],
        x_version = descr_df[["Version"]],
        y = deps,
        type = unlist(type),
        y_raw_version = unlist(version)
    )
}

# extract the required version, if present from description
.extract_version <- function(x) {
    ver <- regmatches(x, gregexpr("\\(.*\\)",x))[[1]]
    if (length(ver) == 0) {
        ver <- "*"
    }
    gsub("\\(|\\)|\\n", "", ver)
}

## dep_df is a terminal node if
## 1. No Import or Depends
## 2. Depends is only R
## 3. Imports/Depends are only unsearchable default packages
## getOption("defaultPackages")
## "datasets", "utils", "grDevices", "graphics", "stats", "methods"

## We should consider Imports, Depends, LinkingTo, and Enhances in normal cases

.generate_disabled_types <- function(no_enhances = TRUE, no_suggests = TRUE) {
    disabled_types <- c()
    if (isTRUE(no_suggests)) {
        disabled_types <- c(disabled_types, "Suggests")
    }
    if (isTRUE(no_enhances)) {
        disabled_types <- c(disabled_types, "Enhances")
    }
    return(disabled_types)
}

.extract_queryable_dependencies <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    if (!"y" %in% colnames(dep_df)) {
        return(NULL)
    }
    disabled_types <- .generate_disabled_types(no_enhances = no_enhances, no_suggests = no_suggests)
    res <- dep_df[!dep_df$type %in% disabled_types &
                  dep_df$y != "R" & !(dep_df$y %in%
                                      c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "tools",
                                        "grid", "splines", "parallel", "stats4", "tcltk", "MASS", "nnet", "class", "spatial",
                                        "eda", "lqs", "mle", "modreg", "mva", "stepfun", "ts")),]
    if (nrow(res) == 0) {
        return(NULL)
    }
    unique(res$y_pkgref)
}

.is_terminal_node <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    length(.extract_queryable_dependencies(dep_df, no_enhances, no_suggests)) == 0
}

## The checking function for below
.generate_bioc_version <- function(snapshot_date, pkgs) {
    bioc_version <- .query_biocver(snapshot_date)$version
    if (length(bioc_version) == 0) {
        return(NULL)
    }
    ## explicit bioc case and too old
    if(any(grepl("^bioc::", pkgs)) && utils::compareVersion(bioc_version, "2.0") == -1) {
        stop("Bioconductor versions < 2.0 are not supported.", call. = FALSE)
    }
    ## old bioc, but not explicit
    if (utils::compareVersion(bioc_version, "2.0") == -1) {
        return(NULL)
    }
    bioc_version
}

.check_local_in_pkgrefs <- function(pkgrefs) {
    res <- .group_pkgrefs_by_source(pkgrefs)
    any_local <- isFALSE(is.null(res[["local"]]))
    if (isTRUE(any_local)) {
        warning("Using \"local\" package(s) to resolve dependencies is not reproducible on another machine.", call. = FALSE)
        for (handle in res[["local"]]) {
            if (isFALSE(.is_directory(handle)) && isFALSE(grepl("\\.tar.gz$|\\.tgz$", handle))) {
                stop(handle, " doesn't appear to be a valid local package.", call. = FALSE)
            }
        }
    }
    invisible()
}

#' Resolve Dependencies Of R Packages
#'
#' This function recursively queries dependencies of R packages at a specific snapshot time. The dependency graph can then be used to recreate the computational environment. The data on dependencies are provided by R-hub.
#'
#' @param pkgs `pkgs` can be 1) a character vector of R packages to resolve, 2) a path to a [`renv` lockfile](https://rstudio.github.io/renv/articles/lockfile.html), or 3) a data structure that [as_pkgrefs()] can convert to a character vector of package references. For 1) `pkgs` can be either in shorthands, e.g. "rtoot", "ropensci/readODS", or in package references, e.g. "cran::rtoot", "github::ropensci/readODS". Please refer to the [Package References documentation](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html) of `pak` for details. Currently, this package supports only cran and github packages. For 2) [as_pkgrefs()] support the output of [sessionInfo()], a renv lockfile or a single directory. If it is a single directory, all R scripts are scanned for R packages used using [renv::dependencies()]. Currently, the default is to scan the R scripts in the current working directory. Please also note that this scanning only assumes there are CRAN and Bioconductor packages. We strongly recommend checking whether this is really the case (see example below).
#' @param snapshot_date Snapshot date, if not specified, assume to be a month ago
#' @param no_enhances logical, whether to ignore packages in the "Enhances" field
#' @param no_suggests logical, whether to ignore packages in the "Suggests" field
#' @param query_sysreqs logical, whether to query for System Requirements. Important: Archived CRAN can't be queried for system requirements. Those
#' packages are assumed to have no system requirement.
#' @param os character, which OS to query for system requirements
#' @param verbose logical, whether to display messages
#' @return a `rang` S3 object with the following items
#' \item{call}{original function call}
#' \item{ranglets}{List of dependency graphs of all packages in `pkgs`}
#' \item{snapshot_date}{`snapshot_date`}
#' \item{no_enhances}{`no_enhances`}
#' \item{no_suggests}{`no_suggests`}
#' \item{unresolved_pkgsrefs}{Packages that can't be resolved}
#' \item{sysreqs}{System requirements as Linux commands}
#' \item{r_version}{The latest R version as of `snapshot_date`}
#' \item{os}{`os`}
#' @export
#' @seealso [dockerize()]
#' @references
#' [Package References](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html)
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                 snapshot_date = "2020-01-16")
#'     graph
#'     ## to resolve github packages
#'     gh_graph <- resolve(pkgs = c("https://github.com/schochastics/rtoot"),
#'                    snapshot_date = "2022-11-28")
#'     gh_graph
#'     ## scanning
#'     graph <- resolve(snapshot_date = "2022-11-28")
#'     ## But we recommend this:
#'     pkgs <- as_pkgrefs(".")
#'     pkgs ## check the accuracy
#'     graph <- resolve(pkgs, snapshot_date = "2022-11-28")
#' }
#' }
resolve <- function(pkgs = ".", snapshot_date, no_enhances = TRUE, no_suggests = TRUE, query_sysreqs = TRUE, os = "ubuntu-20.04", verbose = FALSE) {
    if (!os %in% supported_os) {
        stop("Don't know how to resolve ", os, ". Supported OSes are: ", paste(supported_os, collapse = ", "))
    }
    snapshot_date <- .extract_date(pkgs = pkgs, snapshot_date = snapshot_date, verbose = verbose)
    bioc_version <- .generate_bioc_version(snapshot_date = snapshot_date, pkgs = pkgs)
    pkgrefs <- as_pkgrefs(pkgs, bioc_version = bioc_version, no_enhances = no_enhances,
                          no_suggests = no_suggests)
    .check_local_in_pkgrefs(pkgrefs)
    output <- list()
    output$call <- match.call()
    output$ranglets <- list()
    output$snapshot_date <- snapshot_date
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$unresolved_pkgrefs <- character(0)
    output$sysreqs <- character(0)
    output$r_version <- .query_rver(snapshot_date)
    output$bioc_version <- bioc_version
    output$os <- os
    for (pkgref in pkgrefs) {
        tryCatch({
            res <- .resolve_pkgref(pkgref = pkgref, snapshot_date = snapshot_date, no_enhances = no_enhances,
                                no_suggests = no_suggests, verbose = verbose, bioc_version = bioc_version)
            output$ranglets[[pkgref]] <- res
        }, error = function(err) {
            if (isTRUE(verbose)) {
                cat("Query failed: ", pkgref, "\n")
            }
        })
    }
    output$unresolved_pkgrefs <- setdiff(pkgrefs, names(output$ranglets))
    if (length(output$unresolved_pkgrefs) > 0) {
        warning("Some package(s) can't be resolved: ", paste(output$unresolved_pkgrefs, collapse = ", "), call. = FALSE)
    }
    if (isTRUE(query_sysreqs)) {
        res <- .query_sysreqs(output, os = os)
        output$sysreqs <- res
        .is_ppa_in_sysreqs(output)
    }
    attr(output, "class") <- "rang"
    return(output)
}

.resolve_pkgref <- function(pkgref, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, verbose = FALSE, bioc_version = NULL) {
    pkg_dep_df <- .query_snapshot_dependencies(pkgref = pkgref, snapshot_date = snapshot_date, bioc_version = bioc_version)
    output <- list()
    output$pkgref <- pkgref
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$snapshot_date <- snapshot_date
    output[['original']] <- pkg_dep_df
    output$deps <- list()
    output$unresolved_deps <- character(0)
    q <- fastmap::faststack()
    for (dep in .extract_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests)) {
        q$push(dep)
    }
    seen_deps <- c()
    while (q$size() != 0) {
        current_pkgref <- q$pop()
        if (isTRUE(verbose)) {
            cat("Querying: ", current_pkgref, "\n")
        }
        tryCatch({
            pkg_dep_df <- .query_snapshot_dependencies(pkgref = current_pkgref, snapshot_date = snapshot_date, bioc_version = bioc_version)
            output$deps[[current_pkgref]] <- pkg_dep_df
            pkgs_to_query <- unique(setdiff(.extract_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests), c(names(output$deps), seen_deps)))
            seen_deps <- union(seen_deps, pkgs_to_query)
            for (dep in pkgs_to_query) {
                q$push(dep)
            }
        }, error = function(e) {
            ## can't query
            output$unresolved_deps <<- c(output$unresolved_deps, current_pkgref)
        })
    }
    attr(output, "class") <- "ranglet"
    return(output)
}

#' @export
print.ranglet <- function(x, ...) {
    total_deps <- length(x$deps)
    if (total_deps != 0) {
        total_terminal_nodes <- sum(unlist(lapply(x$deps, .is_terminal_node, no_enhances = x$no_enhances, no_suggests = x$no_suggests)))
    } else {
        total_terminal_nodes <- 0
    }
    latest_version <- unique(x$original$x_version)
    cat("The latest version of `", unique(x$original$x), "` [", .parse_pkgref(x$pkgref, FALSE),  "] at ",
        as.character(x$snapshot_date), " was ", latest_version, ", which has ",
        total_deps, " unique dependencies (", total_terminal_nodes, " with no dependencies.)\n", sep = "")
}

#' @export
print.rang <- function(x, all_pkgs = FALSE, ...) {
    n_ranglets <- length(x$ranglets)
    cat("resolved:", n_ranglets, "package(s). Unresolved package(s):", length(x$unresolved_pkgrefs), "\n")
    if (n_ranglets > 0) {
        if (n_ranglets <= 5 || isTRUE(all_pkgs)) {
            print(x$ranglets)
        } else {
            cat("First 5 packages are:\n")
            print(x$ranglets[seq_len(5)])
        }
    }
}

.extract_latest_modification_date <- function(path, verbose, ignore_use_rang = TRUE) {
    file_paths <- dir(path, recursive = TRUE)
    if (isTRUE(ignore_use_rang)) {
        file_paths <- grep("inst/rang/|Makefile", file_paths, value = TRUE, invert = TRUE)
    }
    if (length(file_paths) == 0) {
        return(NA)
    }
    snapshot_date <- max(file.mtime(file_paths))
    .vcat(verbose, "Based on the latest modification date of files inside the directory: ", snapshot_date)
    return(snapshot_date)
}

## determine the snapshot_date for `resolve` based on `date` and `pkgs`
.extract_date <- function(pkgs, snapshot_date, verbose = FALSE) {
    if (missing(snapshot_date) || is.na(snapshot_date)) {
        .vcat(verbose, "No `snapshot_date`, determining...")
        snapshot_date <- NA
        if (.is_directory(pkgs)) {
            snapshot_date <- .extract_latest_modification_date(path = pkgs, verbose = verbose)
        }
        if (.is_renv_lockfile(pkgs)) {
            snapshot_date <- file.mtime(pkgs)
            .vcat(verbose, "Based on the latest modification date of lockfile: ", snapshot_date)
        }
    }
    if (is.na(snapshot_date)) {
        .vcat(verbose, "Assuming `snapshot_date` to be a month ago.\n")
        snapshot_date <- Sys.Date() - 30
    }
    parsed_snapshot_date <- parsedate::parse_date(snapshot_date)
    if (parsed_snapshot_date > parsedate::parse_date(Sys.time())) {
        stop("We don't know the future.", call. = FALSE)
    }
    parsed_snapshot_date
}


## extract all the pkgrefs of deps and pkgs: for .sysreqs
.extract_pkgrefs <- function(rang) {
    original_pkgrefs <- names(rang$ranglets)
    all_dep_pkgrefs <- unlist(lapply(rang$ranglets, function(x) names(x$deps)))
    unique(c(original_pkgrefs, all_dep_pkgrefs))
}

## returns handles grouped by sources
.group_pkgrefs_by_source <- function(pkgrefs) {
    sources <- vapply(pkgrefs, .parse_pkgref, character(1), return_handle = FALSE, USE.NAMES = FALSE)
    handles <- vapply(pkgrefs, .parse_pkgref, character(1), return_handle = TRUE, USE.NAMES = FALSE)
    res <- list()
    for (source in unique(sources)) {
        res[[source]] <- handles[sources == source]
    }
    return(res)
}

.gh <- function(path,ref = NULL,...){
    url <- httr::parse_url("https://api.github.com/")
    url <- httr::modify_url(url, path = path)
    token <- Sys.getenv("GITHUB_PAT", NA_character_)
    if(is.na(token)){
        token <- Sys.getenv("GITHUB_TOKEN", NA_character_)
    }
    if(is.na(token)){
        token <- ""
    }
    config <- httr::add_headers(Accept = "application/vnd.github.v3+json",Authorization=token)
    params <- list(ref = ref,...)
    request_results <- httr::GET(httr::modify_url(url, path = path), config, query = params)
    status_code <- httr::status_code(request_results)
    if (!status_code %in% c(200)) {
        stop(paste0("github request failed with status code: ", status_code,"\n",
                    "The requested URL was: ",request_results$url), call. = FALSE)
    }
    httr::content(request_results)
}
