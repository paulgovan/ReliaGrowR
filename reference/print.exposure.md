# Print Method for exposure Objects.

Prints a summary of the exposure analysis results.

## Usage

``` r
# S3 method for class 'exposure'
print(x, ...)
```

## Arguments

- x:

  An object of class `exposure`.

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
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
id   <- c(1, 1, 2, 2)
time <- c(100, 200, 150, 300)
result <- exposure(id, time)
print(result)
#> Exposure Analysis for Repairable Systems
#> -----------------------------------------
#> Number of systems:  2
#> Total events:       4
#> Total exposure:     500.00
#> Overall event rate: 0.008000
#> 
#>  Time At Risk Cum. Exposure Cum. Events Event Rate
#>   100       2           200           1   0.005000
#>   150       2           300           2   0.006667
#>   200       2           400           3   0.007500
#>   300       1           500           4   0.008000
```
