% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_rang.R
\name{use_rang}
\alias{use_rang}
\title{Setup rang for a directory}
\usage{
use_rang(
  path = ".",
  add_makefile = TRUE,
  add_here = TRUE,
  verbose = TRUE,
  force = FALSE,
  apptainer = FALSE
)
}
\arguments{
\item{path}{character, path to the project root}

\item{add_makefile}{logical, whether to insert a barebone \code{Makefile} in the project root.}

\item{add_here}{logical, whether to insert a hidden \code{.here} file in the project root}

\item{verbose}{logical, whether to print out messages}

\item{force}{logical, whether to overwrite files (\code{inst/rang/update.R}, \code{Makefile}, \code{.here}) if they
exist.}

\item{apptainer}{logical, whether to use apptainer. \code{FALSE} indicates using Docker}
}
\value{
path, invisibly
}
\description{
This \code{usethis}-style function adds the infrastructure in a directory (presumably with R scripts
and data) for (re)constructing the computational environment.
Specifically, this function inserts \code{inst/rang} into the directory, which contains
all components for the reconstruction. Optionally, \code{Makefile} and \code{.here} are also inserted
to ease the development of analytic code.
By default, (re)running this function does not overwrite any file. One can change this by setting
\code{force} to TRUE.
}
\details{
The infrastructure being added to your path consists of:
\itemize{
\item \code{inst/rang} directory in the project root
\item \code{update.R} file inside the directory
\item \code{.here} in the project root (if \code{add_here} is TRUE)
\item \code{Makefile} in the project root (if \code{add_makefile} is TRUE)
You might need to edit \code{update.R} manually. The default is to scan the whole project for
used R packages and assume they are either on CRAN or Bioconductor. If you have used other R packages,
you might need to edit this manually.
}
}
