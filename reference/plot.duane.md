# Plot Method for Duane Analysis

Generates a Duane plot (log-log or linear scale) with fitted regression
line and optional confidence bounds.

## Usage

``` r
# S3 method for class 'duane'
plot(
  x,
  log = TRUE,
  conf.int = TRUE,
  legend = TRUE,
  legend.pos = "topleft",
  ...
)
```

## Arguments

- x:

  An object of class `"duane"`.

- log:

  Logical; whether to use logarithmic scales for axes (default: `TRUE`).

- conf.int:

  Logical; whether to plot confidence bounds (default: `TRUE`).

- legend:

  Logical; whether to include a legend (default: TRUE).

- legend.pos:

  Position of the legend (default: "topleft").

- ...:

  Further arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures)
plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")
```
