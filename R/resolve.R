.query_rver <- function(snapshot_date) {
    if (snapshot_date < attr(cached_rver, "newest_date")) {
        allvers <- cached_rver
    } else {
        allvers <- .memo_rver()
    }
    allvers$date <- anytime::anytime(allvers$date, tz = "UTC", asUTC = TRUE)
    utils::tail(allvers[allvers$date < snapshot_date,], 1)$version
}

.query_biocver <- function(snapshot_date){
  if (snapshot_date < attr(cached_biocver, "newest_date")) {
    allvers <- cached_biocver
  } else {
    return(data.frame(version="3.16",date = "2022-11-02",rver = 4.2)) # TODO realtime check
  }
  allvers$date <- anytime::anytime(allvers$date, tz = "UTC", asUTC = TRUE)
  utils::tail(allvers[allvers$date < snapshot_date,], 1)[,1:2]
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
.query_snapshot_dependencies <- function(pkgref = "cran::rtoot", snapshot_date = "2022-12-10") {
    source <- .parse_pkgref(pkgref, return_handle = FALSE)
    switch(source,
           "cran" = {
               return(.query_snapshot_dependencies_cran(handle = .parse_pkgref(pkgref) ,snapshot_date = snapshot_date))
           },
           "github" = {
               return(.query_snapshot_dependencies_github(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date))
           },
           "bioc" = {
              return(.query_snapshot_dependencies_bioc(handle = .parse_pkgref(pkgref), snapshot_date = snapshot_date))
           })
}

.query_snapshot_dependencies_cran <- function(handle = "rtoot", snapshot_date = "2022-12-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    search_res <- .memo_search(handle)
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

.query_snapshot_dependencies_github <- function(handle = "schochastics/rtoot", snapshot_date = "2022-12-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    sha <- .query_sha(handle, snapshot_date)
    repo_descr <- gh::gh(paste0("GET /repos/", handle,"/contents/DESCRIPTION"), ref = sha$sha)
    con <- url(repo_descr$download_url)
    descr_df <- as.data.frame(read.dcf(con))
    close(con)
    pkg_dep_df <- .parse_desc(descr_df,snapshot_date)
    pkg_dep_df$x_pkgref <- .normalize_pkgs(handle)
    pkg_dep_df$x_uid <- sha$sha
    pkg_dep_df$x_pubdate <- sha$x_pubdate
    if("y"%in% names(pkg_dep_df)) {
        pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y)
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid", "y", "type", "y_raw_version", "y_pkgref")])
    } else {
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_uid")])
    }
}

.query_snapshot_dependencies_bioc <- function(handle = "BiocGenerics", snapshot_date = "2022-01-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    bioc_version <- .query_biocver(snapshot_date)
    search_res <- .memo_search_bioc(bioc_version$version)
    search_res$pubdate <- anytime::anytime(bioc_version$date, tz = "UTC", asUTC = TRUE)
    
    latest_version <- search_res[search_res$Package==handle,]
    
    if (nrow(latest_version) == 0) {
        stop("No snapshot version exists for ", handle, ".",  call. = FALSE)
    }
    pkg_dep_df <- .parse_desc(descr_df = latest_version,snapshot_date = snapshot_date)
    pkg_dep_df$x_bioc_ver <- bioc_version$version
    pkg_dep_df <- pkg_dep_df[!is.na(pkg_dep_df$y),]
    pkg_dep_df$x_pubdate <- bioc_version$date
    pkg_dep_df$x_pkgref <- .normalize_pkgs(handle,bioc_version = bioc_version$version)
    if("y"%in% names(pkg_dep_df)) {
        pkg_dep_df$y_pkgref <- .normalize_pkgs(pkg_dep_df$y,bioc_version = bioc_version$version)
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_bioc_ver", "y", "type", "y_raw_version", "y_pkgref")])  
    } else {
        return(pkg_dep_df[,c("snapshot_date", "x", "x_version", "x_pubdate", "x_pkgref", "x_bioc_ver")])
    }
}

# get the commit sha for the commit closest to date
.query_sha <- function(handle, date) {
    commits <- gh::gh(paste0("GET /repos/", handle, "/commits"), per_page = 100)
    dates <- sapply(commits,function(x) x$commit$committer$date)
    idx <- which(dates<=date)[1]
    k <- 2
    while(is.null(idx)) {
        commits <- gh::gh(paste0("GET /repos/", handle, "/commits"), per_page = 100, page = k)
        k <- k + 1
    }
    list(sha = commits[[idx]]$sha, x_pubdate =  anytime::anytime(dates[[idx]], tz = "UTC", asUTC = TRUE))
}

# parse a description file from github repo
.parse_desc <- function(descr_df, snapshot_date) {
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
    } else {
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

.extract_queryable_dependencies <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
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
    res <- dep_df[!dep_df$type %in% disabled_types & dep_df$y != "R" & !(dep_df$y %in% c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "tools", "grid", "splines", "Rgraphviz", "parallel", "stats4", "tcltk", "MASS", "nnet", "class", "spatial")),]
    if (nrow(res) == 0) {
        return(NULL)
    } else {
        return(unique(res$y_pkgref))
    }
}

.is_terminal_node <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    length(.extract_queryable_dependencies(dep_df, no_enhances, no_suggests)) == 0
}

#' Resolve Dependencies Of R Packages
#'
#' This function recursively queries dependencies of R packages at a specific snapshot time. The dependency graph can then be used to recreate the computational environment. The data on dependencies are provided by R-hub.
#'
#' @param pkgs `pkgs` can be 1) a character vector of R packages to resolve, or 2) a data structure that [as_pkgrefs()] can convert to a character vector of package references. For 1) `pkgs` can be either in shorthands, e.g. "rtoot", "ropensci/readODS", or in package references, e.g. "cran::rtoot", "github::ropensci/readODS". Please refer to the [Package References documentation](https://r-lib.github.io/pkgdepends/reference/pkg_refs.html) of `pak` for details. Currently, this package supports only cran and github packages. For 2) [as_pkgrefs()] support the output of [sessionInfo()].
#' @param snapshot_date Snapshot date, if not specified, assume to be a month ago
#' @param no_enhances logical, whether to ignore packages in the "Enhances" field
#' @param no_suggests logical, whether to ignore packages in the "Suggests" field
#' @param query_sysreqs logical, whether to query for System Requirements. Important: Archived CRAN can't be queried for system requirements. Those
#' packages are assumed to have no system requirement.
#' @param query_bioc Logical, whether to query for packages from Bioconductor
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
#' }
#' }
resolve <- function(pkgs, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, query_sysreqs = TRUE, query_bioc = FALSE, os = "ubuntu-20.04", verbose = FALSE) {
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
    if (class(pkgs) %in% c("sessionInfo")) {
        pkgrefs <- as_pkgrefs(pkgs)
    } else {
        if(isTRUE(query_bioc)){
            bioc_version <- .query_biocver(snapshot_date)$version
      } else{
            bioc_version <- NULL
      }
      pkgrefs <- .normalize_pkgs(pkgs, bioc_version = bioc_version)
    }
    output <- list()
    output$call <- match.call()
    output$ranglets <- list()
    output$snapshot_date <- snapshot_date
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$unresolved_pkgrefs <- character(0)
    output$sysreqs <- character(0)
    output$r_version <- .query_rver(snapshot_date)
    if(isTRUE(query_bioc)){
        output$bioc_version <- bioc_version
    }
    output$os <- os
    for (pkgref in pkgrefs) {
        tryCatch({
            res <- .resolve_pkgref(pkgref = pkgref, snapshot_date = snapshot_date, no_enhances = no_enhances,
                                no_suggests = no_suggests, verbose = verbose)
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

.resolve_pkgref <- function(pkgref, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, verbose = FALSE) {
    pkg_dep_df <- .query_snapshot_dependencies(pkgref = pkgref, snapshot_date = snapshot_date)
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
            pkg_dep_df <- .query_snapshot_dependencies(pkgref = current_pkgref, snapshot_date = snapshot_date)
            output$deps[[current_pkgref]] <- pkg_dep_df
            pkgs_to_query <- unique(setdiff(.extract_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests), c(names(output$deps), seen_deps)))
            seen_deps <- union(seen_deps, pkgs_to_query)
            for (dep in pkgs_to_query) {
                q$push(dep)
            }
        }, error = function(e) {
            ## can't query
            output$unresolved_deps <- c(output$unresolved_deps, current_pkgref)
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

convert_edgelist <- function(x) {
    output <- data.frame(x = x$pkg, y = .extract_queryable_dependencies(x$original, x$no_enhances, x$no_suggests))
    for (dep in x$deps) {
        if (!.is_terminal_node(dep, x$no_enhances)) {
            el <- data.frame(x = unique(dep$x), y = .extract_queryable_dependencies(dep, x$no_enhances, x$no_suggests))
            output <- rbind(output, el)
        }
    }
    output
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
            result <- query_fun(handle = .parse_pkgref(pkgref), os = os)
            output <- c(output, result)
        }, error = function(e) {
            warning(pkgref, " can't be queried for System requirements. Assumed to have no requirement.", call. = FALSE)
        })
    }
    return(unique(output))
}

## this is vectorized; and for consistency
.query_sysreqs_cran <- function(handle, os) {
    remotes::system_requirements(package = handle, os = os)
}

.query_sysreqs_github <- function(handle, os) {
    res <- lapply(handle, .query_sysreqs_github_single, os = os)
    unique(unlist(res))
}

.query_sysreqs_bioc <- function(handle, os) {
    sys_reqs_all <- .memo_query_sysreqs_rhub()
    pkgs <- .memo_search_bioc(bioc_version = "release") 
    sys_reqs <- .clean_sys_reqs_bioc(pkgs$SystemRequirements[pkgs$Package%in%handle])
    sys_reqs <- sys_reqs[sys_reqs%in%sys_reqs_all]
    if(length(sys_reqs)!=0){
        return(paste("apt-get install -y", sys_reqs))
    } else{
        return(charachter(0))
    }
}

.clean_sys_reqs_bioc <- function(sys_reqs){
  sys_reqs <- unlist(strsplit(sys_reqs,split = ",\\s*|\\n"),use.names = FALSE)
  sys_reqs <- tolower(sys_reqs)
  sys_reqs <- gsub("\\s*\\(.*\\)","",sys_reqs)
  sys_reqs <- gsub("GNU make","gnumake",sys_reqs)
  sys_reqs <- gsub("^gsl$","libgsl",sys_reqs)
  sys_reqs <- gsub("^pandoc.*","pandoc",sys_reqs)
  sys_reqs <- gsub("^xml2$","libxml2",sys_reqs)
}

## get system requirements for github packages
.query_sysreqs_github_single <- function(handle, os) {
    os_info <- strsplit(os, "-")[[1]]
    DEFAULT_RSPM <- "https://packagemanager.rstudio.com"
    DEFAULT_RSPM_REPO_ID <- "1"
    curl <- Sys.which("curl")
    rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
    rspm <- Sys.getenv("RSPM_ROOT", DEFAULT_RSPM)

    rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)
    desc_file <- tempfile()
    ## potential issue: not going back to snapshot time! but the same is true for the remotes approach?
    repo_descr <- gh::gh(paste0("GET /repos/", handle, "/contents/DESCRIPTION"))   
    writeLines(readLines(repo_descr$download_url),con = desc_file)
    res <- system2(
        curl,
        args = c(
            "--silent",
            "--data-binary",
            shQuote(paste0("@", desc_file)),
            shQuote(sprintf("%s/sysreqs?distribution=%s&release=%s&suggests=false",
                            rspm_repo_url,
                            os_info[1],
                            os_info[2])
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
