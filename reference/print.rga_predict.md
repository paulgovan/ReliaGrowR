# Print Method for rga_predict Objects

Prints a formatted table of forecast cumulative failures with confidence
bounds for an `rga_predict` object.

## Usage

``` r
# S3 method for class 'rga_predict'
print(x, ...)
```

## Arguments

- x:

  An object of class `rga_predict`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## See also

Other Reliability Growth Analysis:
[`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md),
[`plot.rga_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga_predict.md),
[`predict_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_rga.md),
[`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md),
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
