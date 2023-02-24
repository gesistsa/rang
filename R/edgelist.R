#' Convert Data Structures to rang edgelist
#'
#' This generic function converts several data structures provided by rang into an edgelist of package dependencies.
#' @param x, supported data structures are `rang` and `ranglet` S3 objects
#' @param ..., not used
#' @return a data frame of directed edges of dependencies
#' @details the resulting data frame can be converted to an igraph object for plotting and analysis via the function [igraph::graph_from_data_frame()]
#' @export
#' @examples
#' \donttest{
#' if (interactive()) {
#'     graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
#'                 snapshot_date = "2020-01-16")
#'                 
#'     # dependency edgelist of a single package
#'     convert_edgelist(graph$ranglets[[1]])
#'     
#'     # full dependency edgelist
#'     convert_edgelist(graph)
#' }
#' }
convert_edgelist <- function(x, ...) {
    UseMethod("convert_edgelist", x)
}

#' @rdname convert_edgelist
#' @export
convert_edgelist.default <- function(x, ...){
    stop(paste("don't know how to convert an object of type",class(x),"to a rang edgelist"), call. = FALSE)
}

#' @rdname convert_edgelist
#' @export
convert_edgelist.ranglet <- function(x, ...){
    output <- data.frame(from = x$pkgref, to = .extract_queryable_dependencies(x$original, x$no_enhances, x$no_suggests))
    for (dep in x$deps) {
        if (!.is_terminal_node(dep, x$no_enhances)) {
          el <- data.frame(from = unique(dep$x_pkgref), to = .extract_queryable_dependencies(dep, x$no_enhances, x$no_suggests))
          output <- rbind(output, el)
       }
    }
    output
}

#' @rdname convert_edgelist
#' @export
convert_edgelist.rang <- function(x, ...){
    if(length(x$ranglets)!=0){
      el <- do.call("rbind",lapply(x$ranglets,convert_edgelist))
      rownames(el) <- NULL
      return(el)
    } else{
      return(data.frame(from = character(0),to = character(0)))
    }

}