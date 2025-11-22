# Print method for duane objects.

This function prints a summary of the Duane analysis result.

## Usage

``` r
# S3 method for class 'duane'
print(x, ...)
```

## Arguments

- x:

  An object of class "duane" returned by the duane_plot function.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## See also

Other Duane functions:
[`duane()`](https://paulgovan.github.io/ReliaGrowR/reference/duane.md),
[`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit <- duane(times, failures)
print(fit)
#> Duane Analysis Result
#> ----------------------
#> Linear model (log-log scale): log(MTBF) ~ log(Time)
#> 
#> Number of observations (failures): 5
#> 
#> Coefficients:
#>                Estimate Std. Error
#> (Intercept)   3.6144974 0.35199619
#> log_cum_times 0.2013244 0.05624037
#> 
#> Log-likelihood: 4.78
#> AIC: -3.55, BIC: -4.72
#> Confidence level: 95.0%
```
