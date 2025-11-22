# P-P Plot for RGA Objects

This function creates a P-P plot for a fitted Reliability Growth
Analysis (RGA) model. Currently only supports the Crow-AMSAA model. A
P-P plot compares the empirical cumulative distribution function (CDF)
to the theoretical CDF specified by the model. If the model fits well,
the points should fall approximately along a straight line.

## Usage

``` r
ppplot.rga(x, main = "P-P Plot", ...)
```

## Arguments

- x:

  An object of class `rga`.

- main:

  Title of the plot.

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

A P-P plot comparing empirical and theoretical CDFs.

## See also

Other goodness-of-fit:
[`qqplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/qqplot.rga.md)

## Examples

``` r
times <- c(5, 10, 15, 20, 25)
failures <- c(1, 2, 1, 3, 2)
fit <- rga(times, failures)
ppplot.rga(fit)
```
