
## one hr

.search <- memoise::memoise(pkgsearch::cran_package_history, cache = cachem::cache_mem(max_age = 60 * 60))
x <- .search("rtoot")

