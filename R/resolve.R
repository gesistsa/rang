
## one hr

.search <- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

## get the latest version as of date
## let's call this output dep_df; basically is a rough version of edgelist
.get_snapshot_dependencies <- function(pkg = "rtoot", snapshot_date = "2022-12-10") {
    snapshot_date <- anytime::anytime(snapshot_date, tz = "UTC", asUTC = TRUE)
    search_res <- .search(pkg)
    ## exclude rows where Version, Date, dependecies are NA
    search_res <- search_res[!is.na(search_res$Date) & !is.na(search_res$Version),]
    search_res$pubdate <- anytime::anytime(search_res$Date, tz = "UTC", asUTC = TRUE)
    snapshot_versions <- search_res[search_res$pubdate <= snapshot_date,]
    best_version <- tail(snapshot_versions[order(snapshot_versions$pubdate),], n = 1)
    dependencies <- best_version$dependencies[[1]]
    data.frame(snapshot_date = snapshot_date, x = pkg, x_version = best_version$Version, y = dependencies$package, type = dependencies$type, y_raw_version = dependencies$version)
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

.is_terminal_node <- function(dep_df) {
    nrow(dep_df[dep_df$type != "Suggests" & dep_df$y != "R" & !(dep_df$y %in% c("datasets", "utils", "grDevices", "graphics", "stats", "methods")),]) == 0
}
