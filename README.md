
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RGA

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/RGA)](https://CRAN.R-project.org/package=RGA)
[![CRAN
checks](https://badges.cranchecks.info/summary/RGA.svg)](https://cran.r-project.org/web/checks/check_results_RGA.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/RGA)](https://cran.r-project.org/package=RGA)
[![](http://cranlogs.r-pkg.org/badges/last-month/RGA)](https://cran.r-project.org/package=RGA)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.RGA-green.svg)](https://doi.org/10.32614/CRAN.package.RGA)
<!-- badges: end -->

## Reliability Growth Analysis via:

- Crow-AMSAA
- Piecewise Weibull NHPP
- Piecewise Weibull NHPP with change point detection

## Installation

To install the release version of RGA, use:

``` r
install_packages('RGA')
```

You can install the development version of RGA like so:

``` r
devtools::install_github('paulgovan/RGA')
```

## Example

Here is a basic example of Reliability Growth Analysis:

First, load the package:

``` r
library(RGA)
```

Next, set up some cumulative time and failure data:

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
```

Then run the RGA and plot the results:

``` r
result <- rga(times, failures)
plot_rga(times, failures, result)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Code of Conduct

Please note that the RGA project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
