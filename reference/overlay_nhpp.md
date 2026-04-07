# Overlay Plot for Multiple NHPP Models

Plots multiple fitted `nhpp` objects on a single set of axes, using
distinct colors per model. Observed data points, fitted lines, and
optional confidence bounds are drawn for every model. Models may have
been fit to different datasets.

## Usage

``` r
overlay_nhpp(
  models,
  conf_bounds = TRUE,
  legend = TRUE,
  legend_pos = "topleft",
  colors = NULL,
  ...
)
```

## Arguments

- models:

  A named or unnamed list of objects of class `nhpp`. At least one model
  must be provided. If the list is named, those names are used as legend
  labels; otherwise labels default to `"Model 1"`, `"Model 2"`, etc.

- conf_bounds:

  Logical; draw confidence bounds for each model (default: `TRUE`).

- legend:

  Logical; draw a legend (default: `TRUE`).

- legend_pos:

  Legend position keyword (default: `"topleft"`).

- colors:

  Optional character vector of colors, one per model. If `NULL`
  (default), [`palette()`](https://rdrr.io/r/grDevices/palette.html)
  colors are cycled.

- ...:

  Additional arguments passed to the initial
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) call (e.g.,
  `main`, `xlab`, `ylab`). Not forwarded to subsequent
  [`lines()`](https://rdrr.io/r/graphics/lines.html) or
  [`points()`](https://rdrr.io/r/graphics/points.html) calls.

## Value

Invisibly returns `NULL`.

## See also

Other Repairable Systems Analysis:
[`exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/exposure.md),
[`mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/mcf.md),
[`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md),
[`plot.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.exposure.md),
[`plot.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.mcf.md),
[`plot.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp.md),
[`plot.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp_predict.md),
[`predict_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_nhpp.md),
[`print.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/print.exposure.md),
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
t1 <- c(200, 400, 600, 800, 1000)
e1 <- c(3, 5, 4, 7, 6)
t2 <- c(300, 600, 900, 1200, 1500)
e2 <- c(4, 6, 5, 8, 7)
m1 <- nhpp(t1, e1)
m2 <- nhpp(t2, e2)
overlay_nhpp(list(System_A = m1, System_B = m2),
  main = "NHPP Overlay", xlab = "Time",
  ylab = "Cumulative Events"
)
```
