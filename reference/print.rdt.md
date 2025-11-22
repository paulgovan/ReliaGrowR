# Print method for rdt objects

This function provides a formatted print method for objects of class
`rdt`.

## Usage

``` r
# S3 method for class 'rdt'
print(x, ...)
```

## Arguments

- x:

  An object of class `rdt`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## Examples

``` r
plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
print(plan)
#> Reliability Demonstration Test (RDT) Plan
#> -----------------------------------------
#> Distribution:  Exponential 
#> Weibull Shape Parameter (Beta):  1 
#> Target Reliability:  0.9 
#> Mission Time:  1000 
#> Input Sample Size (n):  10 
#> Required Test Time (T):  2185.43 
```
