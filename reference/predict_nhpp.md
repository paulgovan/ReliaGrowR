# Forecast Cumulative Events from an NHPP Model.

Takes a fitted `nhpp` object and a vector of future cumulative times,
returning predicted cumulative events with confidence bounds.

## Usage

``` r
predict_nhpp(object, time, conf_level = 0.95)
```

## Arguments

- object:

  An object of class `nhpp` returned by
  [`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md).

- time:

  A numeric vector of cumulative times at which to forecast. All values
  must be finite and \> 0.

- conf_level:

  Confidence level (default 0.95).

## Value

An object of class `nhpp_predict` containing:

- time:

  Forecast times.

- cum_events:

  Predicted cumulative events.

- lower_bounds:

  Lower confidence bounds.

- upper_bounds:

  Upper confidence bounds.

- conf_level:

  Confidence level used.

- model_type:

  Model type.

- nhpp_object:

  The original nhpp object.

## See also

Other Repairable Systems Analysis:
[`exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/exposure.md),
[`mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/mcf.md),
[`nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/nhpp.md),
[`overlay_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/overlay_nhpp.md),
[`plot.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.exposure.md),
[`plot.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.mcf.md),
[`plot.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp.md),
[`plot.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.nhpp_predict.md),
[`print.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/print.exposure.md),
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
time <- c(200, 400, 600, 800, 1000)
event <- c(3, 5, 4, 7, 6)
fit <- nhpp(time, event)
fc <- predict_nhpp(fit, time = c(1500, 2000))
print(fc)
#> NHPP Forecast (Power Law) 
#> -------------------------- 
#>  Time Cum.Events Lower (95%) Upper (95%)
#>  1500       40.9        26.9        62.1
#>  2000       57.8        36.9        90.5
plot(fc, main = "NHPP Forecast", xlab = "Time", ylab = "Cumulative Events")
```
