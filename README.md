
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
graph <- resolve(pkgs = c("rtoot", "rtweet"), snapshot_date = "2022-12-31")
graph
#> resolved: 2 package(s).
#> $rtoot
#> GRAN: The latest version of `rtoot` at 2022-12-31 was 0.2.0, which has 24 unique dependencies (15 with no dependencies.)
#> 
#> $rtweet
#> GRAN: The latest version of `rtweet` at 2022-12-31 was 1.0.2, which has 27 unique dependencies (16 with no dependencies.)
```

-----

1.  Actually, it stands for “GESIS R Archive Network”.
