# Overlay Plot for Multiple RGA Models

Plots multiple fitted `rga` objects on a single set of axes, using
distinct colors per model. Observed data points, fitted lines, and
optional confidence bounds are drawn for every model. Models may have
been fit to different datasets.

## Usage

``` r
overlay_rga(
  models,
  conf_bounds = TRUE,
  legend = TRUE,
  legend_pos = "bottomright",
  colors = NULL,
  log = FALSE,
  ...
)
```

## Arguments

- models:

  A named or unnamed list of objects of class `rga`. At least one model
  must be provided. If the list is named, those names are used as legend
  labels; otherwise labels default to `"Model 1"`, `"Model 2"`, etc.

- conf_bounds:

  Logical; draw confidence bounds for each model (default: `TRUE`).

- legend:

  Logical; draw a legend (default: `TRUE`).

- legend_pos:

  Legend position keyword (default: `"bottomright"`).

- colors:

  Optional character vector of colors, one per model. If `NULL`
  (default), [`palette()`](https://rdrr.io/r/grDevices/palette.html)
  colors are cycled.

- log:

  Logical; use log-log axes (default: `FALSE`).

- ...:

  Additional arguments passed to the initial
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) call (e.g.,
  `main`, `xlab`, `ylab`). Not forwarded to subsequent
  [`lines()`](https://rdrr.io/r/graphics/lines.html) or
  [`points()`](https://rdrr.io/r/graphics/points.html) calls.

## Value

Invisibly returns `NULL`.

## See also

Other Reliability Growth Analysis:
[`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md),
[`plot.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga_predict.md),
[`predict_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_rga.md),
[`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md),
[`print.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga_predict.md),
[`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)

## Examples

``` r
t1 <- c(100, 200, 300, 400, 500)
f1 <- c(1, 2, 1, 3, 2)
t2 <- c(150, 300, 450, 600, 750)
f2 <- c(2, 1, 3, 2, 4)
m1 <- rga(t1, f1)
m2 <- rga(t2, f2)
overlay_rga(list(System_A = m1, System_B = m2),
  main = "RGA Overlay", xlab = "Cumulative Time",
  ylab = "Cumulative Failures"
)
```
