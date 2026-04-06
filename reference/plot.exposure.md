# Plot Method for exposure Objects.

Produces a multi-panel plot of exposure analysis results. The default
layout shows cumulative exposure and cumulative events versus time (top
panel), the number of systems at risk over time (middle panel), and the
event rate over time (bottom panel). Alternatively, a single `which`
panel can be selected.

## Usage

``` r
# S3 method for class 'exposure'
plot(
  x,
  which = c("all", "exposure", "at_risk", "event_rate"),
  legend = TRUE,
  legend_pos = "topleft",
  ...
)
```

## Arguments

- x:

  An object of class `exposure`.

- which:

  Character string selecting which panel(s) to plot. One of `"all"`
  (default), `"exposure"`, `"at_risk"`, or `"event_rate"`.

- legend:

  Logical; show the legend (default: TRUE).

- legend_pos:

  Position of the legend (default: "topleft").

- ...:

  Additional arguments passed to the underlying
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Repairable Systems Analysis:
[`exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/exposure.md),
[`mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/mcf.md),
[`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md),
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
id   <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
time <- c(100, 350, 500, 80, 300, 600, 150, 250, 400, 700)
result <- exposure(id, time)
plot(result)

plot(result, which = "exposure")
```
