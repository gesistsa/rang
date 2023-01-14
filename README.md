
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gran

<!-- badges: start -->

<!-- badges: end -->

The goal of gran (gran: R anachronism nullifier) \[1\] is to obtain the
dependency graph of an R package at a specific time point.

## Installation

You can install the development version of gran like so:

``` r
remotes::install_github("GESIS-Methods-Hub/gran")
```

## Example

To obtain the dependency graph of an R package, use `resolve`

``` r
library(gran)
rtoot_graph <- resolve(pkg = "rtoot", snapshot_date = "2022-12-31")
rtoot_graph
#> GRAN: The latest version of `rtoot` at 2022-12-31 was 0.2.0, which has 26 unique dependencies (15 with no dependencies.)
```

1.  Actually, it stands for “GESIS R Archive Network”.
