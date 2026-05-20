# Plot Method for duane_predict Objects

Plots observed MTBF, fitted Duane curve, and forecast with optional
confidence bounds for a `duane_predict` object.

## Usage

``` r
# S3 method for class 'duane_predict'
plot(x, conf_bounds = TRUE, legend = TRUE, legend_pos = "topleft", ...)
```

## Arguments

- x:

  An object of class `duane_predict`.

- conf_bounds:

  Logical; include confidence bounds (default: `TRUE`).

- legend:

  Logical; show the legend (default: `TRUE`).

- legend_pos:

  Position of the legend (default: `"topleft"`).

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md),
[`predict_duane()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_duane.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md),
[`print.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane_predict.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures)
fc <- predict_duane(fit, times = c(1000, 2000))
#> Warning: Some 'times' values are <= the maximum observed cumulative time. Hindcasting is allowed but may not be meaningful.
plot(fc, main = "Duane Forecast", xlab = "Cumulative Time", ylab = "MTBF")
```
