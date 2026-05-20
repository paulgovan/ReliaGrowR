# Print Method for duane_predict Objects

Prints a formatted table of forecast MTBF with confidence bounds.

## Usage

``` r
# S3 method for class 'duane_predict'
print(x, ...)
```

## Arguments

- x:

  An object of class `duane_predict`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md),
[`plot.duane_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane_predict.md),
[`predict_duane()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_duane.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md)

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
