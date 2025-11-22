# Print method for rga objects.

This function prints a summary of the results from an object of class
`rga`.

## Usage

``` r
# S3 method for class 'rga'
print(x, ...)
```

## Arguments

- x:

  An object of class `rga`, which contains the results from the RGA
  model.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## See also

Other Reliability Growth Analysis:
[`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md),
[`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
result <- rga(times, failures)
print(result)
#> Reliability Growth Analysis (RGA)
#> ---------------------------------
#> Model Type: Crow-AMSAA 
#> 
#> 
#> Number of observations (failures): 5
#> Parameters (per segment):
#>   Growth Rate: 0.2013
#>   Beta: 0.7987 (SE = 0.0562)
#>   Lambda: 0.0269
#> 
#> Goodness of Fit:
#>   Log-likelihood: 4.78
#>   AIC: -3.55
#>   BIC: -4.72
```
