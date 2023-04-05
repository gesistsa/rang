library(rang)
library(here)

## Please note that the project scanning result should be checked manually.
## 1. Github packages must be added manually
## as_pkgrefs(here::here())
## 2. You might also want to change the `snapshot_date`

rang <- resolve(here::here(), snapshot_date = Sys.Date(), verbose = TRUE)

## You might want to edit `post_installation_steps` or `cache`
dockerize(rang, output_dir = here::here(), verbose = TRUE, cache = TRUE,
          post_installation_steps = c("RUN apt-get install make"),
          insert_readme = FALSE,
          copy_all = TRUE)
