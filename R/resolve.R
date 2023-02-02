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
.get_snapshot_dependencies <- function(pkg = "rtoot", snapshot_date = "2022-12-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    search_res <- .search(pkg)
    ## exclude rows where Version, Date, dependecies are NA
    if (is.null(search_res$`Date/Publication`)) {
        search_res$`Date/Publication` <- search_res$Date
    }
    search_res <- search_res[!is.na(search_res$`Date/Publication`) & !is.na(search_res$Version),]
    search_res$pubdate <- anytime::anytime(search_res$`Date/Publication`, tz = "UTC", asUTC = TRUE)
    snapshot_versions <- search_res[search_res$pubdate <= snapshot_date,]
    if (nrow(snapshot_versions) == 0) {
        stop("No snapshot version exists for ", pkg, ".",  call. = FALSE)
    }
    latest_version <- utils::tail(snapshot_versions[order(snapshot_versions$pubdate),], n = 1)
    dependencies <- latest_version$dependencies[[1]]
    if (nrow(dependencies != 0)) {
        return(data.frame(snapshot_date = snapshot_date, x = pkg, x_version = latest_version$Version, x_pubdate = latest_version$pubdate,  y = dependencies$package, type = dependencies$type, y_raw_version = dependencies$version))
    } else {
        ## no y
        return(data.frame(snapshot_date = snapshot_date, x = pkg, x_version = latest_version$Version, x_pubdate = latest_version$pubdate))
    }
}

## dep_df is a terminal node if
## 1. No Import or Depends
## 2. Depends is only R
## 3. Imports/Depends are only unsearchable default packages
## getOption("defaultPackages")
## "datasets", "utils", "grDevices", "graphics", "stats", "methods"

## dbi <- .get_snapshot_dependencies("DBI")
## R6 <- .get_snapshot_dependencies("R6")
## saveRDS(dbi, "tests/testdata/dbi_dep_df.RDS")
## saveRDS(httr, "tests/testdata/httr_dep_df.RDS")
## saveRDS(R6, "tests/testdata/R6.RDS")

## We should consider Imports, Depends, LinkingTo, and Enhances

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
    res <- dep_df[!dep_df$type %in% disabled_types & dep_df$y != "R" & !(dep_df$y %in% c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "tools", "grid", "splines", "Rgraphviz", "parallel", "stats4", "tcltk")),]
    if (nrow(res) == 0) {
        return(NULL)
    } else {
        return(unique(res$y))
    }
}

.is_terminal_node <- function(dep_df, no_enhances = TRUE, no_suggests = TRUE) {
    length(.keep_queryable_dependencies(dep_df, no_enhances, no_suggests)) == 0
}

#' Resolve Dependencies Of R Packages
#'
#' This function recursively queries dependencies of R packages at a specific snapshot time. The dependency graph can then be used to recreate the computing environment. The data on dependencies are provided by R-hub.
#' 
#' @param pkgs character vector of R packages to resolve
#' @param snapshot_date Snapshot date, if not specified, assume to be a month ago
#' @param no_enhances logical, whether to ignore packages in the "Enhances" field
#' @param no_suggests logical, whether to ignore packages in the "Suggests" field
#' @param get_sysreqs logical, whether to query for System Requirements
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
    output <- list()
    output$call <- match.call()
    output$grans <- list()
    output$snapshot_date <- snapshot_date
    output$no_enhances <- no_enhances
    output$no_suggests <- no_suggests
    output$unresolved_pkgs <- character(0)
    output$deps_sysreqs <- character(0)
    output$r_version <- .get_rver(snapshot_date)
    output$os <- os
    for (pkg in pkgs) {
        tryCatch({
            res <- .resolve_pkg(pkg = pkg, snapshot_date = snapshot_date, no_enhances = no_enhances,
                                no_suggests = no_suggests, verbose = verbose)
            output$grans[[pkg]] <- res
        }, error = function(err) {
            if (isTRUE(verbose)) {
                cat("Query failed: ", pkg, "\n")
            }
        })
    }
    output$unresolved_pkgs <- setdiff(pkgs, names(output$grans))
    if (length(output$unresolved_pkgs) > 0) {
        warning("Some package(s) can't be resolved: ", paste(output$unresolved_pkgs, collapse = ", "), call. = FALSE)
    }
    if (isTRUE(get_sysreqs)) {
        res <- .granlist_query_sysreps(output, os = os)
        output$deps_sysreqs <- res
    }
    attr(output, "class") <- "granlist"    
    return(output)
}

.resolve_pkg <- function(pkg, snapshot_date, no_enhances = TRUE, no_suggests = TRUE, verbose = FALSE) {
    pkg_dep_df <- .get_snapshot_dependencies(pkg = pkg, snapshot_date = snapshot_date)
    output <- list()
    output$pkg <- pkg
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
        current_pkg <- q$pop()
        if (isTRUE(verbose)) {
            cat("Querying: ", current_pkg, "\n")
        }
        tryCatch({
            pkg_dep_df <- .get_snapshot_dependencies(pkg = current_pkg, snapshot_date = snapshot_date)
            output$deps[[current_pkg]] <- pkg_dep_df
            pkgs_need_query <- unique(setdiff(.keep_queryable_dependencies(pkg_dep_df, no_enhances, no_suggests), c(names(output$dep), seen_deps)))
            seen_deps <- union(seen_deps, pkgs_need_query)
            for (dep in pkgs_need_query) {
                q$push(dep)
            }
        }, error = function(e) {
            ## can't query
            output$unresolved_deps <- c(output$unresolved_deps, current_pkg)
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
    cat("The latest version of `", x$pkg, "` at ", as.character(x$snapshot_date), " was ", latest_version, ", which has ", total_deps, " unique dependencies (", total_terminal_nodes, " with no dependencies.)\n", sep = "")
}

#' @export
print.granlist <- function(x, all_pkgs = FALSE, ...) {
    n_grans <- length(x$grans)
    cat("resolved:", n_grans, "package(s). Unresolved package(s): ", length(x$unresolved_pkgs), "\n")
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
    remotes::system_requirements(package = targets, os = os) ## don't gp me
}

## os <- names(remotes:::supported_os_versions())
## supported_os <- unlist(mapply(function(x, y) paste(x,"-", y, sep = ""), os, remotes:::supported_os_versions()))
## names(supported_os) <- NULL
## usethis::use_data(supported_os, internal = TRUE)
