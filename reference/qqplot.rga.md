# Q-Q Plot for RGA Objects

This function creates a Q-Q plot for a fitted Reliability Growth
Analysis (RGA) model Currently only supports the Crow-AMSAA model. A Q-Q
plot compares the quantiles of the empirical data to the quantiles of
the theoretical distribution specified by the model. If the model fits
well, the points should fall approximately along a straight line.

## Usage

``` r
qqplot.rga(x, main = "Q-Q Plot", ...)
```

## Arguments

- x:

  An object of class `rga`.

- main:

  Title of the plot.

- ...:

  Additional arguments passed to
  [`stats::qqplot()`](https://rdrr.io/r/stats/qqnorm.html).

## Value

A Q-Q plot comparing empirical and theoretical quantiles.

## See also

Other goodness-of-fit:
[`ppplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/ppplot.rga.md)

## Examples

``` r
times <- c(5, 10, 15, 20, 25)
failures <- c(1, 2, 1, 3, 2)
fit <- rga(times, failures)
qqplot.rga(fit)
```
