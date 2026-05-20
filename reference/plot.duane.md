# Plot Method for Duane Analysis

Generates a Duane plot (log-log or linear scale) with fitted regression
line and optional confidence bounds.

## Usage

``` r
# S3 method for class 'duane'
plot(
  x,
  log = TRUE,
  conf_bounds = TRUE,
  legend = TRUE,
  legend_pos = "topleft",
  conf.int = NULL,
  legend.pos = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"duane"`.

- log:

  Logical; whether to use logarithmic scales for axes (default: `TRUE`).

- conf_bounds:

  Logical; whether to plot confidence bounds (default: `TRUE`).

- legend:

  Logical; whether to include a legend (default: TRUE).

- legend_pos:

  Position of the legend (default: "topleft").

- conf.int:

  Deprecated. Use `conf_bounds` instead.

- legend.pos:

  Deprecated. Use `legend_pos` instead.

- ...:

  Further arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`plot.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane_predict.md),
[`predict_duane()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_duane.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md),
[`print.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane_predict.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures)
plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")
```
