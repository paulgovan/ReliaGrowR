# Plot Method for nhpp Objects.

Plots observed cumulative events with the fitted NHPP model and optional
confidence bounds.

## Usage

``` r
# S3 method for class 'nhpp'
plot(x, conf_bounds = TRUE, legend = TRUE, legend_pos = "topleft", ...)
```

## Arguments

- x:

  An object of class `nhpp`.

- conf_bounds:

  Logical; include confidence bounds (default: TRUE).

- legend:

  Logical; show the legend (default: TRUE).

- legend_pos:

  Position of the legend (default: "topleft").

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Repairable Systems Analysis:
[`exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/exposure.md),
[`mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/mcf.md),
[`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md),
[`overlay_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/overlay_nhpp.md),
[`plot.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.exposure.md),
[`plot.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.mcf.md),
[`plot.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp_predict.md),
[`predict_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_nhpp.md),
[`print.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/print.exposure.md),
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
time <- c(200, 400, 600, 800, 1000)
event <- c(3, 5, 4, 7, 6)
result <- nhpp(time, event)
plot(result, main = "Power Law NHPP", xlab = "Time", ylab = "Cumulative Events")
```
