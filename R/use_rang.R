#' Setup rang for a directory
#'
#' This `usethis`-style function adds the infrastructure in a directory (presumably with R scripts
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

#' Create executable research compendium according to the Turing Way
#'
#' This `usethis`-style function creates an executable research compendium according to the Turing Way.
#' @param path character, path to the project root
#' @param add_rang logical, whether to run [use_rang()] to `path`
#' @param ... additional parameters pass to [use_rang()]
#' @return path, invisibly
#' @seealso [use_rang()]
#' @details
#' According to the Turing Way, an executable research compendium should have the following properties
#' 1. Files should be organized in a conventional folder structure;
#' 2. Data, methods, and output should be clearly separated;
#' 3. The computational environment should be specified.
#'
#' We use the structure suggested by the Turing Way:
#' * `data_raw`: a directory to hold the raw data
#' * `data_clean`: a directory to hold the processed data
#' * `code`: a directory to hold computer code
#' * `CITATION`: a file holding citation information
#' * `paper.Rmd`: a manuscript

#' This function provides the a clearly separated organizational structure. Components can be changed. For example, the manuscript can be in another format (e.g. quarto, sweave) or even optional. With `add_rang`, the computational environment can be recorded and reconstructed later.
#'
#' @references
#' [The Turing Way: Research Compendia](https://the-turing-way.netlify.app/reproducible-research/compendia.html)
#' Gorman, KB, Williams TD. and Fraser WR (2014). [Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis)](http://dx.doi.org/10.1371/journal.pone.0090081). PLoS ONE 9(3):e90081.
#' @export
create_turing <- function(path, add_rang = TRUE, ...) {
    if (isTRUE(dir.exists(path))) {
        stop("`path` exists.")
    }
    dir.create(path)
    file.copy(from = list.files(system.file("turing", package = "rang"), full.names = TRUE),
              to = path, recursive = TRUE)
    writeLines(c("Please cite this research compendium as:", "", "    <CITATION INFORMATION HERE>"), file.path(path, "CITATION"))
    dir.create(file.path(path, "figures"))
    dir.create(file.path(path, "data_clean"))
    if (isTRUE(add_rang)) {
        use_rang(path, ...)
    }
    invisible(path)
}
