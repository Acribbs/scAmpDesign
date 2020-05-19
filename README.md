
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scAmpDesign

<!-- badges: start -->

<!-- badges: end -->

The goal of scAmpDesign is to design primers for single-cell sequencing.

## Installation

You can install the released version of scAmpDesign from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acribbs/scAmpDesign")
```

## Example

This is a basic example which shows you how scAmpDesign should be used:

``` r
library(scAmpDesign)
single_primer_design(ncbi="NC_045512", primer3="/Users/adamcribbs/miniconda3/bin/primer3_core")
#> [1] "Looks like there may be a problem, did you specify a real NCBI reference?\n                                                       Otherwise GenBank may be down"
#> [1] "Looks like there may be a problem, did you specify a real Ensembl reference?\n                                                     Otherwise the server may be down. If an output is produced please ignore this warning"
```
