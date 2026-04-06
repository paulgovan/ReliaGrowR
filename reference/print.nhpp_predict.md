# Print Method for nhpp_predict Objects.

Prints a formatted table of forecast cumulative events with confidence
bounds.

## Usage

``` r
# S3 method for class 'nhpp_predict'
print(x, ...)
```

## Arguments

- x:

  An object of class `nhpp_predict`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

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
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md)

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
```
