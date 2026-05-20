# Forecast MTBF from a Duane Model

Takes a fitted `duane` object and a vector of cumulative times,
returning predicted MTBF with confidence bounds as a `duane_predict` S3
object.

## Usage

``` r
predict_duane(object, times, conf_level = 0.95)
```

## Arguments

- object:

  An object of class `duane` returned by
  [`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md).

- times:

  A numeric vector of cumulative times at which to forecast MTBF. All
  values must be finite and \> 0. A warning is issued if any value is at
  or below the maximum observed cumulative time (hindcasting).

- conf_level:

  The desired confidence level (default `0.95`). Must be a single finite
  numeric in (0, 1).

## Value

An object of class `duane_predict` containing:

- times:

  The forecast cumulative times.

- mtbf:

  Predicted MTBF values.

- lower_bounds:

  Lower confidence bounds on MTBF.

- upper_bounds:

  Upper confidence bounds on MTBF.

- conf_level:

  The confidence level used.

- duane_object:

  The original `duane` object (used by the plot method).

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md),
[`plot.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane_predict.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md),
[`print.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane_predict.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures)
fc <- predict_duane(fit, times = c(1000, 2000))
#> Warning: Some 'times' values are <= the maximum observed cumulative time. Hindcasting is allowed but may not be meaningful.
print(fc)
#> Duane MTBF Forecast 
#> -------------------- 
#>  Time   MTBF Lower (95%) Upper (95%)
#>  1000 149.19      120.39      184.87
#>  2000 171.53      126.35      232.86
```
