#' @importFrom memoise memoise
#' @importFrom pkgsearch cran_package_history
#' @importFrom remotes system_requirements
NULL

## one hr

.cran_package_history <- function(package, max_retries = 5) {
    n_retries <- 0
    while(n_retries < max_retries) {
        tryCatch({
            return(pkgsearch::cran_package_history(package))
        }, error = function(e) {
            if (grepl("parse error: premature EOF", e$message)) {
                n_retries <<- n_retries + 1
                ##message("retrying in 2s...")
                Sys.sleep(2)
            } else {
                stop(e)
            }
        })
    }
    stop("Can't query this package: ", package, call. = FALSE)
}

.memo_search <- memoise::memoise(.cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))

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

.vcat <- function(verbose = TRUE, ...) {
    if (isTRUE(verbose)) {
        message(..., "\n")
    }
    invisible()
}

## data generation
## ---
## recipes <- list()
## recipes["texlive"] <- "## install texlive\napt-get install -y pandoc pandoc-citeproc texlive"
## recipes["texlivefull"] <- "## install texlive-full\napt-get install -y pandoc pandoc-citeproc texlive-full"
## recipes["quarto"] <- "## install quarto (latest)\napt-get install -y curl git && curl -LO https://quarto.org/download/latest/quarto-linux-amd64.deb && dpkg -i quarto-linux-amd64.deb && quarto install tool tinytex && rm quarto-linux-amd64.deb"
## recipes["clean"] <- "## Clean up caches\nrm -rf /var/lib/apt/lists/* && if [ -d \"$CACHE_PATH\" ]; then rm -rf $CACHE_PATH; fi"
## recipes["make"] <- "## install GNU make\napt-get -y install make"
## usethis::use_data(recipes, overwrite = TRUE)

#' Recipes for Building Container Images
#'
#' A list containing several useful recipes for container building. Useful for the `post_installation_steps` argument of [dockerize()]. Available recipes are:
#' * `texlive`: install pandoc and LaTeX, useful for rendering RMarkdown
#' * `texlivefull`: Similar to the above, but install the full distribution of TeX Live (~ 3GB)
#' * `quarto`: install quarto and tinytex
#' * `clean`: clean up the container image by removing cache
#' * `make`: install GNU make
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                     snapshot_date = "2020-01-16")
#'     ## install texlive
#'     dockerize(graph, ".", post_installation_steps = recipes[['texlive']])
#' }
#' }
"recipes"

## internal data generation
## ---
## ### Supported OS Versions
## supported_os <- c("trusty" = "ubuntu-14.04", "xenial" = "ubuntu-16.04", "bionic" = "ubuntu-18.04", "focal" = "ubuntu-20.04", "centos-6", "centos-7", "centos-8", "redhat-6", "redhat-7", "redhat-8")
## ### R version history
## cached_rver <- .rver()
## attr(cached_rver, "newest_date") <- anytime::anytime(tail(cached_rver, n = 1)$date, tz = "UTC", asUTC = TRUE)
## ### Bioconductor version history
## cached_biocver <- .biocver()
## attr(cached_biocver, "newest_date") <- max(cached_biocver$date)
## debian_version <- c("lenny", "squeeze", "wheezy", "jessie", "stretch")
## .get_debian_urls <- function(debian_version, output_dir, verbose) {
##     sha <- .gh(paste0("/repos/debuerreotype/docker-debian-eol-artifacts/branches/dist-",
##                       debian_version))$commit$sha
##     .gh(paste0("/repos/debuerreotype/docker-debian-eol-artifacts/contents/",
##                debian_version, "/amd64/rootfs.tar.xz"), ref = sha)$download_url
## }
## debian_urls <- sapply(debian_version, .get_debian_urls)
## usethis::use_data(supported_os, cached_rver, cached_biocver, debian_urls, internal = TRUE, overwrite = TRUE)

## test data upgrade
## ---
## devtools::load_all()
## all_rds_files <- list.files(here::here("tests/testdata"), ".RDS", full.names = TRUE)
## for (rds in all_rds_files) {
##     x <- readRDS(rds)
##     y <- eval(x$call)
##     saveRDS(y, rds)
## }
