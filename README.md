
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReliaGrowR

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/ReliaGrowR)](https://CRAN.R-project.org/package=ReliaGrowR)
[![R-CMD-check](https://github.com/paulgovan/ReliaGrowR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/paulgovan/ReliaGrowR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/paulgovan/ReliaGrowR/graph/badge.svg)](https://app.codecov.io/gh/paulgovan/ReliaGrowR)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ReliaGrowR)](https://cran.r-project.org/package=ReliaGrowR)
[![](http://cranlogs.r-pkg.org/badges/last-month/ReliaGrowR)](https://cran.r-project.org/package=ReliaGrowR)
[![](https://img.shields.io/badge/doi-10.32614/CRAN.package.ReliaGrowR-green.svg)](https://doi.org/10.32614/CRAN.package.ReliaGrowR)
<!-- badges: end -->

## Introduction

Welcome to **ReliaGrowR**! This package provides modeling and plotting
functions for Reliability Growth Analysis (RGA), including:

- Duane Analysis
- Crow-AMSAA NHPP
- Piecewise Weibull NHPP
- Piecewise Weibull NHPP with Change Point Detection

To learn more about RGA and this package, please view the
[vignette](https://paulgovan.github.io/ReliaGrowR/articles/RGA.html).

## Installation

To install the release version of ReliaGrowR, use:

``` r
install_packages('ReliaGrowR')
```

You can install the development version of ReliaGrowR like so:

``` r
devtools::install_github('paulgovan/ReliaGrowR')
```

## Example

Here is a basic example of Reliability Growth Analysis:

First, load the package:

``` r
library(ReliaGrowR)
```

Next, set up some cumulative time and failure data:

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
```

Then run the rga and plot the results:

``` r
result <- rga(times, failures)
plot(result, main = "Reliability Growth Analysis", xlab = "Cumulative Time", ylab = "Cumulative Failures")
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## Code of Conduct

Please note that the ReliaGrowR project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
