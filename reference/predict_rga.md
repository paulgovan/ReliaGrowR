# Forecast Cumulative Failures from a Reliability Growth Model

Takes a fitted `rga` object and a vector of cumulative times, returning
predicted cumulative failures with confidence bounds as an `rga_predict`
S3 object.

## Usage

``` r
predict_rga(object, times, conf_level = 0.95)
```

## Arguments

- object:

  An object of class `rga` returned by
  [`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md).

- times:

  A numeric vector of cumulative times at which to forecast. All values
  must be finite and \> 0. A warning is issued if any value is at or
  below the maximum observed cumulative time (hindcasting).

- conf_level:

  The desired confidence level (default `0.95`). Must be a single finite
  numeric in (0, 1).

## Value

An object of class `rga_predict` containing:

- times:

  The forecast cumulative times.

- cum_failures:

  Predicted cumulative failures.

- lower_bounds:

  Lower confidence bounds.

- upper_bounds:

  Upper confidence bounds.

- conf_level:

  The confidence level used.

- model_type:

  Either `"Crow-AMSAA"` or `"Piecewise NHPP"`.

- rga_object:

  The original `rga` object (used by the plot method).

## See also

Other Reliability Growth Analysis:
[`overlay_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/overlay_rga.md),
[`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md),
[`plot.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga_predict.md),
[`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md),
[`print.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga_predict.md),
[`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- rga(times, failures)
fc <- predict_rga(fit, times = c(1500, 2000))
#> Warning: Some 'times' values are <= the maximum observed cumulative time. Hindcasting is allowed but may not be meaningful.
print(fc)
#> Reliability Growth Forecast (Crow-AMSAA) 
#> ----------------------------------------- 
#>  Time Cum.Failures Lower (95%) Upper (95%)
#>  1500          9.3         7.1        12.1
#>  2000         11.7         8.6        15.8
```
