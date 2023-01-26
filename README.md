
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gran

<!-- badges: start -->

<!-- badges: end -->

The goal of gran (gran: R anachronism nullifier) \[1\] is to obtain the
dependency graph of R packages at a specific time point.

## Installation

You can install the development version of gran like so:

``` r
remotes::install_github("chainsawriot/gran")
```

## Example

To obtain the dependency graph of an R package, use `resolve`

``` r
library(gran)
graph <- resolve(pkgs = c("openNLP", "LDAvis", "topicmodels", "quanteda"), snapshot_date = "2020-01-16")
graph
```

``` r
graph$deps_sysreqs
#> [1] "apt-get install -y default-jdk" "apt-get install -y libxml2-dev"
#> [3] "apt-get install -y make"        "apt-get install -y zlib1g-dev" 
#> [5] "apt-get install -y libpng-dev"  "apt-get install -y libgsl0-dev"
#> [7] "apt-get install -y python3"     "apt-get install -y libicu-dev"
```

``` r
graph$r_version
#> [1] "3.6.2"
```

## Recreate the computing environment

The object can be used to recreate the computing environment via
[Rocker](https://github.com/rocker-org/rocker).

``` r
make_docker(graph, "~/rocker_test")
```

Now, you can build and run the Docker container.

``` bash
cd rocker_test
sudo docker build -t gran .
sudo docker run --rm --name "grantest" -ti gran
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

-----

1.  Actually, it stands for “GESIS R Archive Network”.
