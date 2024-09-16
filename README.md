
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/rga)](https://CRAN.R-project.org/package=rga)
[![CRAN
checks](https://badges.cranchecks.info/summary/rga.svg)](https://cran.r-project.org/web/checks/check_results_rga.html)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rga)](https://cran.r-project.org/package=rga)
[![](http://cranlogs.r-pkg.org/badges/last-month/rga)](https://cran.r-project.org/package=rga)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.rga-green.svg)](https://doi.org/10.32614/CRAN.package.rga)
<!-- badges: end -->

## Reliability Growth Analysis via:

- Crow-AMSAA
- Piecewise Weibull NHPP
- Piecewise Weibull NHPP with change point detection

## Installation

To install the release version of rga, use:

``` r
install_packages('rga')
```

You can install the development version of rga like so:

``` r
devtools::install_github('paulgovan/rga')
```

## Example

Here is a basic example of Reliability Growth Analysis:

First, load the package:

``` r
library(rga)
```

Next, set up some cumulative time and failure data:

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
```

Then run the rga and plot the results:

``` r
result <- rga(times, failures)
plot_rga(result)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Code of Conduct

Please note that the rga project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
