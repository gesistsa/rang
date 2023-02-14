#' @importFrom memoise memoise
#' @importFrom pkgsearch cran_package_history
NULL

## one hr

.memo_search<- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

.rver <- function() {
    suppressWarnings(jsonlite::fromJSON(readLines("https://api.r-hub.io/rversions/r-versions"), simplifyVector = TRUE))
}

.memo_rver <- memoise::memoise(.rver, cache = cachem::cache_mem(max_age = 120 * 60))

## internal data generation
## ---
## os <- names(remotes:::supported_os_versions())
## supported_os <- unlist(mapply(function(x, y) paste(x,"-", y, sep = ""), os, remotes:::supported_os_versions()))
## names(supported_os) <- NULL
## cached_rver <- .rver()
## attr(cached_rver, "newest_date") <- anytime::anytime(tail(cached_rver, n = 1)$date, tz = "UTC", asUTC = TRUE)
## usethis::use_data(supported_os, cached_rver, internal = TRUE, overwrite = TRUE)

## test data upgrade
## ---
## devtools::load_all()
## all_rds_files <- list.files(here::here("tests/testdata"), ".RDS", full.names = TRUE)
## for (rds in all_rds_files) {
##     x <- readRDS(rds)
##     y <- eval(x$call)
##     saveRDS(y, rds)
## }
