
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rang <img src="man/figures/rang_logo.svg" align="right" height="200" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/chainsawriot/rang/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chainsawriot/rang/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rang (Reconstructing Ancient Number-crunching Gears) \[1\]
is to obtain the dependency graph of R packages at a specific time
point.

Although this package can also be used to ensure the current R
computational environment can be reconstructed by future researchers,
this package gears towards reconstructing historical R computational
environments which have not been completely declared. For the former
purpose, packages such as [renv](https://github.com/rstudio/renv/),
[groundhog](https://github.com/CredibilityLab/groundhog),
[miniCRAN](https://github.com/andrie/miniCRAN), and
[Require](https://github.com/PredictiveEcology/Require) should be used.
One can think of rang as an archaeological tool.

To reconstruct a historical R computational environment, this package
assumes only the availability of source packages on CRAN. The
reconstruction procedures have been tested in several vintage versions
of R.

Please cite this package as:

Chan CH, Schoch D (2023) rang: Reconstructing reproducible R
computational environments. arXiv
preprint:[2303.04758](https://doi.org/10.48550/arXiv.2303.04758)

## Installation

You can install the development version of rang like so:

``` r
remotes::install_github("chainsawriot/rang")
```

Or the stable CRAN version

``` r
install.packages("rang")
```

## Example

To obtain the dependency graph of R packages, use `resolve`. Currently,
this package supports both CRAN and Github packages.

``` r
library(rang)
x <- resolve(pkgs = c("sna", "schochastics/rtoot"), snapshot_date = "2022-11-30")
```

``` r
graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"),
                 snapshot_date = "2020-01-16")
```

``` r
graph
#> resolved: 4 package(s). Unresolved package(s): 0 
#> $`cran::openNLP`
#> The latest version of `openNLP` [cran] at 2020-01-16 was 0.2-7, which has 3 unique dependencies (2 with no dependencies.)
#> 
#> $`cran::LDAvis`
#> The latest version of `LDAvis` [cran] at 2020-01-16 was 0.3.2, which has 2 unique dependencies (2 with no dependencies.)
#> 
#> $`cran::topicmodels`
#> The latest version of `topicmodels` [cran] at 2020-01-16 was 0.2-9, which has 7 unique dependencies (5 with no dependencies.)
#> 
#> $`cran::quanteda`
#> The latest version of `quanteda` [cran] at 2020-01-16 was 1.5.2, which has 63 unique dependencies (33 with no dependencies.)
```

``` r
graph$sysreqs
#> [1] "apt-get install -y default-jdk" "apt-get install -y libxml2-dev"
#> [3] "apt-get install -y make"        "apt-get install -y zlib1g-dev" 
#> [5] "apt-get install -y libpng-dev"  "apt-get install -y libgsl0-dev"
#> [7] "apt-get install -y libicu-dev"  "apt-get install -y python3"
```

``` r
graph$r_version
#> [1] "3.6.2"
```

The resolved result is an S3 object called `rang` and can be exported as
an installation script. The installation script can be execute on a
vanilla R installation.

``` r
export_rang(graph, "rang.R")
```

However, the execution of the installation script often fails (now) due
to missing system dependencies and incompatible R versions. Therefore,
the approach outlined below should be used.

## Recreate the computational environment via Rocker

A `rang` object can be used to recreate the computational environment
via [Rocker](https://github.com/rocker-org/rocker). Please note that the
oldest R version one can get from Rocker is R 3.1.0.

``` r
dockerize(graph, "~/rocker_test")
```

Now, you can build and run the Docker container.

``` bash
cd ~/rocker_test
docker build -t rang .
docker run --rm --name "rangtest" -ti rang
```

Using the above example, `sessionInfo()` outputs the following. You have
successfully gone back to the pre-pandemic time.

``` 
R version 3.6.2 (2019-12-12)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 10 (buster)

Matrix products: default
BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.3.5.so

locale:
 [1] LC_CTYPE=en_US.UTF-8          LC_NUMERIC=C                 
 [3] LC_TIME=en_US.UTF-8           LC_COLLATE=en_US.UTF-8       
 [5] LC_MONETARY=en_US.UTF-8       LC_MESSAGES=C                
 [7] LC_PAPER=en_US.UTF-8          LC_NAME=en_US.UTF-8          
 [9] LC_ADDRESS=en_US.UTF-8        LC_TELEPHONE=en_US.UTF-8     
[11] LC_MEASUREMENT=en_US.UTF-8    LC_IDENTIFICATION=en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] topicmodels_0.2-9 LDAvis_0.3.2      openNLP_0.2-7     quanteda_1.5.2   

loaded via a namespace (and not attached):
 [1] NLP_0.2-0           Rcpp_1.0.3          pillar_1.4.3       
 [4] compiler_3.6.2      tools_3.6.2         stopwords_1.0      
 [7] lubridate_1.7.4     lifecycle_0.1.0     tibble_2.1.3       
[10] gtable_0.3.0        lattice_0.20-38     pkgconfig_2.0.3    
[13] rlang_0.4.2         Matrix_1.2-18       fastmatch_1.1-0    
[16] parallel_3.6.2      openNLPdata_1.5.3-4 rJava_0.9-11       
[19] xml2_1.2.2          stringr_1.4.0       stats4_3.6.2       
[22] grid_3.6.2          data.table_1.12.8   R6_2.4.1           
[25] ggplot2_3.2.1       spacyr_1.2          magrittr_1.5       
[28] scales_1.1.0        modeltools_0.2-22   colorspace_1.4-1   
[31] stringi_1.4.5       RcppParallel_4.4.4  lazyeval_0.2.2     
[34] munsell_0.5.0       tm_0.7-7            slam_0.1-47        
[37] crayon_1.3.4    
```

### Caching R packages

One can also cache (or archive) the R packages from CRAN and Github at
the time `dockerize` is executed. The cached R packages will then
transfer to the container. Please note that system requirements
(i.e. `deb` packages) are not cached.

``` r
dockerize(graph, "~/rocker_test", cache = TRUE)
```

### Using alternative Rocker images

One can also select other Rocker versioned images: `rstudio`,
`tidyverse`, `verse`, `geospatial`.

``` r
dockerize(graph, "~/rocker_test", image = "rstudio")
```

`tidyverse`, `verse`, and `geospatial` are similar to the default
(`r-ver`). For `rstudio`, one needs to build and launch it with:

``` bash
cd ~/rocker_test
docker build -t rang .
docker run -p 8787:8787 -e PASSWORD=abc123 --rm --name "rangtest" -ti rang
```

With any browser, go to: `local:8787`. The default username is
`rstudio`, password is as specified.

## Recreate the computational environment for R \< 3.1.0

`rang` can still be used to recreate computational environments for R \<
3.1.0. The Dockerfile generated is based on Debian Lenny (5.0) and the
requested version of R is compiled from source. As of writing, this
method works for R \< 3.1.0 but not R \< 1.3.1. The `image` parameter is
ignored in this case.

``` r
rang_rio <- resolve("rio", snapshot_date = "2013-08-28") ## R 3.0.1
dockerize(rang_rio, output_dir = "~/old_renviron")
```

## Acknowledgment

The logo of rang is a remix of
[this](https://commons.wikimedia.org/wiki/File:Flag_of_the_Canary_Islands.svg)
public domain image. The two dogs should be *Presa Canario*, the native
dog breed on the islands of Gran Canaria and Tenerife.

-----

1.  It stands for “R Archiving Nerds at GESIS”. The package was
    previously named `gran`, but we decided to rename it to `rang`
    because there is another package named
    [gRAN](https://CRAN.R-project.org/package=GRANBase).
