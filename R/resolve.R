
## one hr

.search <- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

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

.keep_queryable_dependencies <- function(dep_df, no_enhances = FALSE) {
    if (!"y" %in% colnames(dep_df)) {
        return(NULL)
    }
    if (isTRUE(no_enhances)) {
        disabled_types <- c("Suggests", "Enhances")
    } else {
        disabled_types <- c("Suggests")
    }
    res <- dep_df[!dep_df$type %in% disabled_types & dep_df$y != "R" & !(dep_df$y %in% c("datasets", "utils", "grDevices", "graphics", "stats", "methods", "tools", "grid", "splines", "Rgraphviz", "parallel", "stats4")),]
    if (nrow(res) == 0) {
        return(NULL)
    } else {
        return(unique(res$y))
    }
}

.is_terminal_node <- function(dep_df, no_enhances = TRUE) {
    length(.keep_queryable_dependencies(dep_df, no_enhances)) == 0
}

## pkg <- "rtoot"
## snapshot_date <- "2022-12-10"

#' @export
resolve <- function(pkg, snapshot_date, no_enhances = TRUE) {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    if (snapshot_date >= anytime::anytime(Sys.Date())) {
        stop("We don't know the future.", call. = FALSE)
    }
    pkg_dep_df <- .get_snapshot_dependencies(pkg = pkg, snapshot_date = snapshot_date)
    output <- list()
    output$pkg <- pkg
    output$no_enhances <- no_enhances
    output$snapshot_date <- snapshot_date
    output[['original']] <- pkg_dep_df
    output$dep <- list()
    q <- dequer::deque()
    for (dep in .keep_queryable_dependencies(pkg_dep_df, no_enhances)) {
        dequer::pushback(q, dep)
    }
    seen_deps <- c()
    while (length(q) != 0) {
        current_pkg <- dequer::pop(q)
        pkg_dep_df <- .get_snapshot_dependencies(pkg = current_pkg, snapshot_date = snapshot_date)
        output$dep[[current_pkg]] <- pkg_dep_df
        pkgs_need_query <- unique(setdiff(.keep_queryable_dependencies(pkg_dep_df, no_enhances), c(names(output$dep), seen_deps)))
        seen_deps <- union(seen_deps, pkgs_need_query)
        for (dep in pkgs_need_query) {
            dequer::pushback(q, dep)
        }
    }
    attr(output, "class") <- "gran"
    return(output)
}

#' @export
print.gran <- function(x, ...) {
    total_deps <- length(x$dep)
    if (total_deps != 0) {
        total_terminal_nodes <- sum(unlist(lapply(x$dep, .is_terminal_node, no_enhances = x$no_enhances)))
    } else {
        total_terminal_nodes <- 0
    }
    latest_version <- unique(x$original$x_version)
    cat("GRAN: The latest version of `", x$pkg, "` at ", as.character(x$snapshot_date), " was ", latest_version, ", which has ", total_deps, " unique dependencies (", total_terminal_nodes, " with no dependencies.)\n", sep = "")
}

#' @export
convert_edgelist <- function(x) {
    output <- data.frame(x = x$pkg, y = .keep_queryable_dependencies(x$original, x$no_enhances))
    for (dep in x$dep) {
        if (!.is_terminal_node(dep, x$no_enhances)) {
            el <- data.frame(x = unique(dep$x), y = .keep_queryable_dependencies(dep, x$no_enhances))
            output <- rbind(output, el)
        }
    }
    output
}
