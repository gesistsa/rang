library(rang)
library(here)

cran_mirror <- "https://cloud.r-project.org/"

## Please note that the project scanning result should be checked manually.
## 1. Github packages must be added manually
## as_pkgrefs(here::here())
## 2. You might also want to change the `snapshot_date` to a fix date, when
## the project is finalized.

rang <- resolve(here::here(),
                snapshot_date = NA,
                verbose = TRUE)

## You might want to edit `post_installation_steps` or `cache`
apptainerize(rang, output_dir = here::here(), verbose = TRUE, cache = TRUE,
             post_installation_steps = c(recipes[["make"]], recipes[["texlive"]], recipes[["clean"]]),
             insert_readme = FALSE,
             copy_all = TRUE,
             cran_mirror = cran_mirror)
