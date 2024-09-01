
<!-- README.md is generated from README.Rmd. Please edit that file -->

## RGA: Reliability Growth Analysis in R

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/RGA)](https://CRAN.R-project.org/package=RGA)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of RGA like so:

``` r
devtools::install_github('paulgovan/RGA')
```

## Example

Here is a basic example of Reliability Growth Analysis:

``` r
library(RGA)
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
result <- rga(times, failures)
plot_rga(times, failures, result)
```

<img src="man/figures/README-example-1.png" width="100%" />

## Code of Conduct

Please note that the RGA project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
