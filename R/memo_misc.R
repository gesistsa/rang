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

.biocver <- function(){
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
    bioc_date <- anytime::anytime(sub(pattern, "\\2", map), tz = "UTC", asUTC = TRUE)
    data.frame(version = bioc_ver,date=bioc_date)
}

.memo_biocver <- memoise::memoise(.biocver, cache = cachem::cache_mem(max_age = 120 * 60))


.bioc_package_history <- function(bioc_version){
    if(bioc_version>="2.0"){
        con <- url(paste0("http://bioconductor.org/packages/",bioc_version,"/bioc/VIEWS"))
        pkgs <- read.dcf(con)
        close(con)
    } else{
        stop("Bioconductor versions <2.0 are not supported")
    }
    as.data.frame(pkgs)
}

.memo_search_bioc <- memoise::memoise(.bioc_package_history, cache = cachem::cache_mem(max_age = 60 * 60)) 

.query_sysreqs_rhub <- function(){
  jsonlite::fromJSON("https://sysreqs.r-hub.io/list")
}

.memo_query_sysreqs_rhub <- memoise::memoise(.query_sysreqs_rhub,cache = cachem::cache_mem(max_age = 60 * 60))
## internal data generation
## ---
## os <- names(remotes:::supported_os_versions())
## supported_os <- unlist(mapply(function(x, y) paste(x,"-", y, sep = ""), os, remotes:::supported_os_versions()))
## names(supported_os) <- NULL
## cached_rver <- .rver()
## attr(cached_rver, "newest_date") <- anytime::anytime(tail(cached_rver, n = 1)$date, tz = "UTC", asUTC = TRUE)
## usethis::use_data(supported_os, cached_rver, internal = TRUE, overwrite = TRUE)

##library(rvest)
##doc <- read_html("https://www.bioconductor.org/about/release-announcements/")
##cached_biocver <- html_table(doc)[[1]]
##cached_biocver$Date <- anytime::anytime(cached_biocver$Date, tz = "UTC", asUTC = TRUE)
##cached_biocver$`Software packages` <- NULL
##names(cached_biocver) <- c("version","date","rver")
##cached_biocver <- cached_biocver[order(cached_biocver$date),]
##cached_biocver <- as.data.frame(cached_biocver)



## test data upgrade
## ---
## devtools::load_all()
## all_rds_files <- list.files(here::here("tests/testdata"), ".RDS", full.names = TRUE)
## for (rds in all_rds_files) {
##     x <- readRDS(rds)
##     y <- eval(x$call)
##     saveRDS(y, rds)
## }
