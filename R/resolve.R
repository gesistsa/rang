#' @importFrom memoise memoise
#' @importFrom pkgsearch cran_package_history
NULL

## one hr

.search <- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

.rver <- function() {
    suppressWarnings(jsonlite::fromJSON(readLines("https://api.r-hub.io/rversions/r-versions"), simplifyVector = TRUE))
}

.memo_rver <- memoise::memoise(.rver, cache = cachem::cache_mem(max_age = 120 * 60))

.get_rver <- function(snapshot_date) {
    allvers <- .memo_rver()
    allvers$date <- anytime::anytime(allvers$date, tz = "UTC", asUTC = TRUE)
    utils::tail(allvers[allvers$date < snapshot_date,], 1)$version
}



## .msysreps <- memoise::memoise(.raw_sysreqs, cache = cachem::cache_mem(max_age = 60 * 60))

## .sysreps <- function(pkg, verbose = FALSE) {
##     if (isTRUE(verbose)) {
##         cat("Querying SystemRequirements of", pkg, "\n")
##     }
##     .msysreps(pkg)
## }

## get the latest version as of date
## let's call this output dep_df; basically is a rough version of edgelist
.get_snapshot_dependencies <- function(pkgref = "cran::rtoot", snapshot_date = "2022-12-10") {
    source <- .parse_pkgref(pkgref, return_handle = FALSE)
    switch(source,
           "cran" = {
               return(.get_snapshot_dependencies_cran(handle = .parse_pkgref(pkgref) ,snapshot_date = snapshot_date))
           },
           "github" = {
               return(.get_snapshot_dependencies_gh(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date))
           })
}

.get_snapshot_dependencies_cran <- function(handle = "rtoot", snapshot_date = "2022-12-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    search_res <- .search(handle)
    search_res$pubdate <- anytime::anytime(search_res$crandb_file_date, tz = "UTC", asUTC = TRUE)
    snapshot_versions <- search_res[search_res$pubdate <= snapshot_date,]
    if (nrow(snapshot_versions) == 0) {
        stop("No snapshot version exists for ", handle, ".",  call. = FALSE)
    }
    latest_version <- utils::tail(snapshot_versions[order(snapshot_versions$pubdate),], n = 1)
    dependencies <- latest_version$dependencies[[1]]
    if (nrow(dependencies != 0)) {
        return(data.frame(snapshot_date = snapshot_date, x = handle, x_version = latest_version$Version, x_pubdate = latest_version$pubdate, x_pkgref = .normalize_pkgs(handle), y = dependencies$package, type = dependencies$type, y_raw_version = dependencies$version, y_pkgref = .normalize_pkgs(dependencies$package)))
    } else {
        ## no y
        return(data.frame(snapshot_date = snapshot_date, x = handle, x_version = latest_version$Version, x_pubdate = latest_version$pubdate, x_pkgref = .normalize_pkgs(handle)))
    }
}

.get_snapshot_dependencies_gh <- function(handle = "schochastics/rtoot", snapshot_date = "2022-12-10"){
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    sha <- .get_sha(handle, snapshot_date)
    repo_descr <- gh::gh(paste0("GET /repos/", handle,"/contents/DESCRIPTION"), ref = sha$sha)
    con <- url(repo_descr$download_url)
    descr_df <- as.data.frame(read.dcf(con))
    close(con)
    pkg_dep_df <- .parse_desc(descr_df,snapshot_date)
    pkg_dep_df$x_pkgref <- .normalize_pkgs(handle)
    pkg_dep_df$x_uid <- sha$sha
    pkg_dep_df$x_pubdate <- sha$x_pubdate
    if("y"%in% names(pkg_dep_df)){
        pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y)
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid", "y", "type", "y_raw_version", "y_pkgref")])  
    } else{
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid")])
    }
}

# get the commit sha for the commit closest to date
.get_sha <- function(handle, date){
    commits <- gh::gh(paste0("GET /repos/", handle, "/commits"), per_page = 100)
    dates <- sapply(commits,function(x) x$commit$committer$date)
    idx <- which(dates<=date)[1]
    k <- 2
    while(is.null(idx)){
        commits <- gh::gh(paste0("GET /repos/", handle, "/commits"), per_page = 100, page = k)  
        k <- k + 1
    }
    list(sha = commits[[idx]]$sha, x_pubdate =  anytime::anytime(dates[[idx]], tz = "UTC", asUTC = TRUE))
}

# parse a description file from github repo
.parse_desc <- function(descr_df, snapshot_date){
    types <- c("Depends","LinkingTo","Imports","Suggests","Enhances")
    depends <- descr_df[["Depends"]]
    imports <- descr_df[["Imports"]]
    linking <- descr_df[["LinkingTo"]]
    suggests <- descr_df[["Suggests"]]
    enhances <- descr_df[["Enhances"]]
    if(!is.null(imports))  imports <- strsplit(imports, ",[\n]*")[[1]]
    if(!is.null(linking))  linking <- strsplit(linking, ",[\n]*")[[1]]
    if(!is.null(suggests)) suggests <- strsplit(suggests, ",[\n]*")[[1]]
    if(!is.null(enhances)) enhances <- strsplit(enhances, ",[\n]*")[[1]]
    if(!is.null(depends))  depends <- strsplit(depends, ",[\n]*")[[1]]
    raw_deps <- list(
        depends, linking, imports, suggests, enhances
    )
    type <- lapply(seq_along(raw_deps), function(x) rep(types[x], length(raw_deps[[x]])))
    version <- vapply(unlist(raw_deps), .extract_version, character(1), USE.NAMES = FALSE)
    deps <- gsub("\\s*\\(.*\\)","",unlist(raw_deps))
    if(length(deps) != 0) {
        return(data.frame(
            snapshot_date = snapshot_date,
            x = descr_df[["Package"]],
            x_version = descr_df[["Version"]],
            y = deps,
            type = unlist(type),
            y_raw_version = unlist(version)
        ))
    } else{
        return(data.frame(
            snapshot_date = as.Date(snapshot_date),
            x = descr_df[["Package"]],
            x_version = descr_df[["Version"]]
        ))
    }
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

.keep_queryable_dependencies <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    if (!"y" %in% colnames(dep_df)) {
        return(NULL)
    }
    disabled_types <- c()
    if (isTRUE(no_suggests)) {
        disabled_types <- c(disabled_types, "Suggests")
    }
    if (isTRUE(no_enhances)) {
        disabled_types <- c(disabled_types, "Enhances")
    }
    res <- dep_df[!dep_df$type %in% disabled_types & dep_df$y != "R" & !(dep_df$y %in% c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "tools", "grid", "splines", "Rgraphviz", "parallel", "stats4", "tcltk", "MASS", "nnet")),]
    if (nrow(res) == 0) {
        return(NULL)
    } else {
        return(unique(res$y_pkgref))
    }
}

.is_terminal_node <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    length(.keep_queryable_dependencies(dep_df, no_enhances, no_suggests)) == 0
}

#' Resolve Dependencies Of R Packages
#'
#' This function recursively queries dependencies of R packages at a specific snapshot time. The dependency graph can then be used to recreate the computational environment. The data on dependencies are provided by R-hub.
#' 
#' @param pkgs character vector of R packages to resolve
#' @param snapshot_date Snapshot date, if not specified, assume to be a month ago
#' @param no_enhances logical, whether to ignore packages in the "Enhances" field
#' @param no_suggests logical, whether to ignore packages in the "Suggests" field
#' @param get_sysreqs logical, whether to query for System Requirements. Important: Archived CRAN can't be queried for system requirements. Those
#' packages are assumed to have no system requirement.
#' @param os character, which OS to query for system requirements
#' @param verbose logical, whether to display messages
#' @return S3 object `granlist`
#' @export
#' @seealso [dockerize()]
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                 snapshot_date = "2020-01-16")
#'     graph
#' }
#' }
resolve <- function(pkgs, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, get_sysreqs = TRUE, os = "ubuntu-20.04", verbose = FALSE) {
    if (!os %in% supported_os) {
        stop("Don't know how to resolve ", os, ". Supported OSes are: ", paste(supported_os, collapse = ", "))
    }
    if (missing(snapshot_date)) {
        if (isTRUE(verbose)) {
            cat("No `snapshot_date`: Assuming `snapshot_date` to be a month ago.\n")
        }
        snapshot_date <- Sys.Date() - 30
    }
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    if (snapshot_date >= anytime::anytime(Sys.Date())) {
        stop("We don't know the future.", call. = FALSE)
    }
    pkgrefs <- .normalize_pkgs(pkgs)
    output <- list()
    output$call <- match.call()
    output$grans <- list()
    output$snapshot_date <- snapshot_date
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$unresolved_pkgrefs <- character(0)
    output$deps_sysreqs <- character(0)
    output$r_version <- .get_rver(snapshot_date)
    output$os <- os
    for (pkgref in pkgrefs) {
        tryCatch({
            res <- .resolve_pkgref(pkgref = pkgref, snapshot_date = snapshot_date, no_enhances = no_enhances,
                                no_suggests = no_suggests, verbose = verbose)
            output$grans[[pkgref]] <- res
        }, error = function(err) {
            if (isTRUE(verbose)) {
                cat("Query failed: ", pkgref, "\n")
            }
        })
    }
    output$unresolved_pkgrefs <- setdiff(pkgrefs, names(output$grans))
    if (length(output$unresolved_pkgrefs) > 0) {
        warning("Some package(s) can't be resolved: ", paste(output$unresolved_pkgrefs, collapse = ", "), call. = FALSE)
    }
    if (isTRUE(get_sysreqs)) {
        res <- .granlist_query_sysreps(output, os = os)
        output$deps_sysreqs <- res
        .has_ppa_in_sysreqs(output)
    }
    attr(output, "class") <- "granlist"
    return(output)
}

.resolve_pkgref <- function(pkgref, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, verbose = FALSE) {
    pkg_dep_df <- .get_snapshot_dependencies(pkgref = pkgref, snapshot_date = snapshot_date)
    output <- list()
    output$pkgref <- pkgref
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$snapshot_date <- snapshot_date
    output[['original']] <- pkg_dep_df
    output$deps <- list()
    output$unresolved_deps <- character(0)
    q <- fastmap::faststack()
    for (dep in .keep_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests)) {
        q$push(dep)
    }
    seen_deps <- c()
    while (q$size() != 0) {
        current_pkgref <- q$pop()
        if (isTRUE(verbose)) {
            cat("Querying: ", current_pkgref, "\n")
        }
        tryCatch({
            pkg_dep_df <- .get_snapshot_dependencies(pkgref = current_pkgref, snapshot_date = snapshot_date)
            output$deps[[current_pkgref]] <- pkg_dep_df
            pkgs_to_query <- unique(setdiff(.keep_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests), c(names(output$deps), seen_deps)))
            seen_deps <- union(seen_deps, pkgs_to_query)
            for (dep in pkgs_to_query) {
                q$push(dep)
            }
        }, error = function(e) {
            ## can't query
            output$unresolved_deps <- c(output$unresolved_deps, current_pkgref)
        })
    }
    attr(output, "class") <- "gran"
    return(output)
}

#' @export
print.gran <- function(x, ...) {
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
print.granlist <- function(x, all_pkgs = FALSE, ...) {
    n_grans <- length(x$grans)
    cat("resolved:", n_grans, "package(s). Unresolved package(s):", length(x$unresolved_pkgs), "\n")
    if (n_grans > 0) {
        if (n_grans <= 5 || isTRUE(all_pkgs)) {
            print(x$grans)
        } else {
            cat("First 5 packages are:\n")
            print(x$grans[seq_len(5)])
        }
    }
}

convert_edgelist <- function(x) {
    output <- data.frame(x = x$pkg, y = .keep_queryable_dependencies(x$original, x$no_enhances, x$no_suggests))
    for (dep in x$deps) {
        if (!.is_terminal_node(dep, x$no_enhances)) {
            el <- data.frame(x = unique(dep$x), y = .keep_queryable_dependencies(dep, x$no_enhances, x$no_suggests))
            output <- rbind(output, el)
        }
    }
    output
}

## extract all the names of deps and pkgs: for .sysdeps
.granlist_extract_all_deps <- function(granlist) {
    pkgs <- names(granlist$grans)
    all_deps <- unlist(lapply(granlist$grans, function(x) names(x$deps)))
    unique(c(pkgs, all_deps))
}

.granlist_query_sysreps <- function(granlist, os = "ubuntu-20.04") {
    targets <- .granlist_extract_all_deps(granlist)
    if (length(targets) == 0) {
        warning("No packages to query for system requirements.", call. = FALSE)
        return(NA)
    }
    tryCatch({
        return(remotes::system_requirements(package = targets, os = os))
    }, error = function(e) {
        return(.query_sysreps_safe(targets = targets, os = os))
    })
}

.query_sysreps_safe <- function(targets, os = "ubuntu-20.04") {
  output <- c()
  for (pkg in targets) {
    if(isFALSE(.is_github(pkg))){
      tryCatch({
        result <- remotes::system_requirements(package = pkg, os = os)
        output <- c(output, result)
      }, error = function(e) {
        warning(pkg, " can't be queried for System requirements. Assumed to have no requirement.", call. = FALSE)
      })
    } else{
      tryCatch({
        result <- system_requirements_gh(package = pkg,os = os)
        output <- c(output, result)
      }, error = function(e) {
        warning(pkg, " can't be queried for System requirements. Assumed to have no requirement.", call. = FALSE)
      })
    }
  }
  return(unique(output))
}

# get system requirements for github packages
DEFAULT_RSPM <- "https://packagemanager.rstudio.com"
DEFAULT_RSPM_REPO_ID <- "1"
system_requirements_gh <- function(package, os){
  curl = Sys.which("curl")
  
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)
  
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)
  desc_file <- tempfile()
  # potenital issue: not going back to snapshot time! but the same is true for the remotes approach?
  repo_descr <- gh::gh(paste0("GET /repos/",package,"/contents/DESCRIPTION")) 
  
  writeLines(readLines(repo_descr$download_url),con = desc_file)
  res <- system2(
    curl,
    args = c(
      "--silent",
      "--data-binary",
      shQuote(paste0("@", desc_file)),
      shQuote(sprintf("%s/sysreqs?distribution=%s&release=%s&suggests=true",
                      rspm_repo_url,
                      "ubuntu",
                      "20.04")
      )
    ),
    stdout = TRUE
  )
  file.remove(desc_file)
  res <- json$parse(res)
  if (!is.null(res$error)) {
    stop(res$error)
  }
  unique(unlist(c(res[["install_scripts"]], 
                  lapply(res[["dependencies"]], `[[`, "install_scripts"))))
}

## os <- names(remotes:::supported_os_versions())
## supported_os <- unlist(mapply(function(x, y) paste(x,"-", y, sep = ""), os, remotes:::supported_os_versions()))
## names(supported_os) <- NULL
## usethis::use_data(supported_os, internal = TRUE)
