# Print Method for mcf Objects.

Prints a summary of the Mean Cumulative Function results.

## Usage

``` r
# S3 method for class 'mcf'
print(x, ...)
```

## Arguments

- x:

  An object of class `mcf`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

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
[`predict_nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/predict_nhpp.md),
[`print.exposure()`](https://paulgovan.github.io/ReliaGrowR/reference/print.exposure.md),
[`print.nhpp()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
id <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
time <- c(100, 300, 500, 150, 400, 50, 200, 350, 600)
result <- mcf(id, time)
print(result)
#> Mean Cumulative Function (MCF)
#> ------------------------------
#> Number of systems: 3
#> Number of events:  9
#> Total exposure:    1500.00
#> Confidence level:  95%
#> 
#>  Time    MCF Lower (95%) Upper (95%)
#>    50 0.3333      0.0000      0.9867
#>   100 0.6667      0.0000      1.5906
#>   150 1.0000      0.0000      2.1316
#>   200 1.3333      0.0267      2.6400
#>   300 1.6667      0.2058      3.1275
#>   350 2.0000      0.3997      3.6003
#>   400 2.3333      0.6048      4.0619
#>   500 2.8333      0.8463      4.8203
#>   600 3.8333      1.0423      6.6243
```
