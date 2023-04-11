#' Setup rang for a directory
#'
#' This function adds the infrastructure in a directory (presumably with R scripts
#' and data) for (re)constructing the computational environment.
#' Specifically, this function inserts `inst/rang` into the directory, which contains
#' all components for the reconstruction. Optionally, `Makefile` and `.here` are also inserted
#' to ease the development of analytic code.
#' By default, (re)running this function does not overwrite any file. One can change this by setting
#' `force` to TRUE.
#' @param path character, path to the project root
#' @param add_makefile logical, whether to insert a barebone `Makefile` in the project root.
#' @param add_here logical, whether to insert a hidden `.here` file in the project root
#' @param verbose logical, whether to print out messages
#' @param force logical, whether to overwrite files (`inst/rang/update.R`, `Makefile`, `.here`) if they
#' exist.
#' @return path, invisibly
#' @details The infrastructure being added to your path consists of:
#' * `inst/rang` directory in the project root
#' * `update.R` file inside the directory
#' * `.here` in the project root (if `add_here` is TRUE)
#' * `Makefile` in the project root (if `add_makefile` is TRUE)
#' You might need to edit `update.R` manually. The default is to scan the whole project for
#' used R packages and assume they are either on CRAN or Bioconductor. If you have used other R packages,
#' you might need to edit this manually.
#' @export
use_rang <- function(path = ".", add_makefile = TRUE, add_here = TRUE,
                     verbose = TRUE, force = FALSE) {
    if (isFALSE(dir.exists(path))) {
        stop("'path' does not exist")
    }
    base_dir <- file.path(path, "inst/rang")
    if (isFALSE(dir.exists(base_dir))) {
        dir.create(base_dir, recursive = TRUE)
        .vcat(verbose, "`inst/rang` created.")
    } else {
        .vcat(verbose, "`inst/rang` exists.")
    }
    if (isFALSE(file.exists(file.path(base_dir, "update.R"))) || isTRUE(force)) {
        file.copy(system.file("update.R", package = "rang"), file.path(base_dir, "update.R"), overwrite = TRUE)
    }
    if (isTRUE(add_makefile) && (isFALSE(file.exists(file.path(path, "Makefile"))) || isTRUE(force))) {
        file.copy(system.file("Makefile", package = "rang"), file.path(path, "Makefile"), overwrite = TRUE)
        .vcat(verbose, "`Makefile` added.")
    }
    if (isTRUE(add_here) && (isFALSE(file.exists(file.path(path, ".here"))) || isTRUE(force))) {
        file.create(file.path(path, ".here"))
        .vcat(verbose, "`.here` added.")
    }
    .vcat(verbose, "The infrastructure for running `rang` in this project is now ready.")
    .vcat(verbose, "You might want to edit this file: inst/rang/update.R")
    .vcat(verbose, paste0("After that, run: setwd(\"", path,"\"); source(\"inst/rang/update.R\")"))
    .vcat(verbose && add_makefile, "Or run in your shell: make update")
    return(invisible(path))
}
