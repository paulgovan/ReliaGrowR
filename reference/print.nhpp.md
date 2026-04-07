# Print Method for nhpp Objects.

Prints a summary of the NHPP model results.

## Usage

``` r
# S3 method for class 'nhpp'
print(x, ...)
```

## Arguments

- x:

  An object of class `nhpp`.

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
[`print.mcf()`](https://paulgovan.github.io/ReliaGrowR/reference/print.mcf.md),
[`print.nhpp_predict()`](https://paulgovan.github.io/ReliaGrowR/reference/print.nhpp_predict.md)

## Examples

``` r
time <- c(200, 400, 600, 800, 1000)
event <- c(3, 5, 4, 7, 6)
result <- nhpp(time, event)
print(result)
#> Non-Homogeneous Poisson Process (NHPP)
#> ---------------------------------------
#> Model Type: Power Law 
#> Estimation Method: MLE 
#> Number of observations: 5
#> 
#> Parameters:
#>   Beta: 1.2014 (SE = 0.1181)
#>   Lambda: 0.0063
#> 
#> Goodness of Fit:
#>   Log-likelihood: 15.87
#>   AIC: -27.75
#>   BIC: -28.53
```
