# Plot Method for mcf Objects.

Plots the Mean Cumulative Function with optional confidence bounds.

## Usage

``` r
# S3 method for class 'mcf'
plot(x, conf_bounds = TRUE, legend = TRUE, legend_pos = "topleft", ...)
```

## Arguments

- x:

  An object of class `mcf`.

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
[`plot.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp.md),
[`plot.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp_predict.md),
[`predict_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_nhpp.md),
[`print.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/print.exposure.md),
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
id <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
time <- c(100, 300, 500, 150, 400, 50, 200, 350, 600)
result <- mcf(id, time)
plot(result, main = "Mean Cumulative Function")
```
