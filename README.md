
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gran

<!-- badges: start -->

<!-- badges: end -->

The goal of gran (gran: R anachronism nullifier) \[1\] is to obtain the
dependency graph of R packages at a specific time point.

## Installation

You can install the development version of gran like so:

``` r
remotes::install_github("GESIS-Methods-Hub/gran")
```

## Example

To obtain the dependency graph of an R package, use `resolve`

``` r
library(gran)
graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"), snapshot_date = "2020-01-16")
graph
#> resolved: 4 package(s).
#> $openNLP
#> GRAN: The latest version of `openNLP` at 2020-01-16 was 0.2-7, which has 3 unique dependencies (2 with no dependencies.)
#> 
#> $LDAvis
#> GRAN: The latest version of `LDAvis` at 2020-01-16 was 0.3.2, which has 2 unique dependencies (2 with no dependencies.)
#> 
#> $topicmodels
#> GRAN: The latest version of `topicmodels` at 2020-01-16 was 0.2-9, which has 7 unique dependencies (5 with no dependencies.)
#> 
#> $quanteda
#> GRAN: The latest version of `quanteda` at 2020-01-16 was 1.5.2, which has 64 unique dependencies (34 with no dependencies.)
```

``` r
graph$deps_sysreqs
#> [1] "apt-get install -y default-jdk" "apt-get install -y libxml2-dev"
#> [3] "apt-get install -y make"        "apt-get install -y zlib1g-dev" 
#> [5] "apt-get install -y libpng-dev"  "apt-get install -y libgsl0-dev"
#> [7] "apt-get install -y python3"     "apt-get install -y libicu-dev"
```

-----

1.  Actually, it stands for “GESIS R Archive Network”.
