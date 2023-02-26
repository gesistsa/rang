#' @importFrom memoise memoise
#' @importFrom pkgsearch cran_package_history
#' @importFrom remotes system_requirements
NULL

## one hr

.memo_search <- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

.rver <- function() {
    suppressWarnings(jsonlite::fromJSON(readLines("https://api.r-hub.io/rversions/r-versions"), simplifyVector = TRUE))
}

.memo_rver <- memoise::memoise(.rver, cache = cachem::cache_mem(max_age = 120 * 60))

.biocver <- function() {
    url <- "https://bioconductor.org/config.yaml"
    tag <- "release_dates"
    txt <- readLines(url)
    grps <- grep("^[^[:blank:]]", txt)
    start <- match(grep(tag, txt), grps)
    end <- ifelse(length(grps) < start + 1, length(txt), grps[start + 1] - 1)
    map <- txt[seq(grps[start] + 1, end)]
    map <- trimws(gsub("\"", "", sub(" #.*", "", map)))
    pattern <- "(.*): (.*)"
    bioc_ver <- sub(pattern, "\\1", map)
    bioc_date <- parsedate::parse_date(sub(pattern, "\\2", map))
    data.frame(version = bioc_ver, date=bioc_date)
}

.memo_biocver <- memoise::memoise(.biocver, cache = cachem::cache_mem(max_age = 120 * 60))

.bioc_package_history <- function(bioc_version) {
    if (bioc_version != "release" && utils::compareVersion(bioc_version, "2.0") == -1) {
        stop("Bioconductor versions < 2.0 are not supported.", call. = FALSE)
    }
    suffixes <- c("bioc", "data/annotation", "data/experiment", "workflow")
    output <- data.frame()
    for (suffix in suffixes) {
        view_url <- paste0("http://bioconductor.org/packages/", bioc_version, "/", suffix, "/VIEWS")
        con <- url(view_url)
        tryCatch({
            raw_metadata <- suppressWarnings(read.dcf(con))
            metadata <- as.data.frame(raw_metadata)
            metadata$suffix <- suffix
            output <- vctrs::vec_rbind(output, metadata)
            close(con)
        }, error = function(e) {
            close(con)
        })
    }
    output
}

.memo_search_bioc <- memoise::memoise(.bioc_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

## internal data generation
## ---
### Supported OS Versions
## os <- names(remotes:::supported_os_versions())
## supported_os <- unlist(mapply(function(x, y) paste(x,"-", y, sep = ""), os, remotes:::supported_os_versions()))
## names(supported_os) <- NULL
### R version history
## cached_rver <- .rver()
## attr(cached_rver, "newest_date") <- anytime::anytime(tail(cached_rver, n = 1)$date, tz = "UTC", asUTC = TRUE)
### Bioconductor version history
## cached_biocver <- .biocver()
## attr(cached_biocver, "newest_date") <- max(cached_biocver$date)
## usethis::use_data(supported_os, cached_rver, cached_biocver, internal = TRUE, overwrite = TRUE)

## test data upgrade
## ---
## devtools::load_all()
## all_rds_files <- list.files(here::here("tests/testdata"), ".RDS", full.names = TRUE)
## for (rds in all_rds_files) {
##     x <- readRDS(rds)
##     y <- eval(x$call)
##     saveRDS(y, rds)
## }
