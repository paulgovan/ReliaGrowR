# Print Method for gof Objects

Prints a summary of goodness-of-fit statistics.

## Usage

``` r
# S3 method for class 'gof'
print(x, ...)
```

## Arguments

- x:

  An object of class `gof`.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns the input object.

## See also

Other goodness-of-fit:
[`gof()`](https://paulgovan.github.io/ReliaGrowR/reference/gof.md),
[`ppplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/ppplot.rga.md),
[`qqplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/qqplot.rga.md)

## Examples

``` r
times <- c(5, 10, 15, 20, 25)
failures <- c(1, 2, 1, 3, 2)
fit <- rga(times, failures)
g <- gof(fit)
print(g)
#> Goodness-of-Fit Statistics (Crow-AMSAA)
#> ------------------------------------------ 
#> n (observations): 5
#> 
#> Cramer-von Mises statistic (W^2): 0.02835
#> Kolmogorov-Smirnov statistic (D):  0.12347
#> 
#> Smaller values indicate a better fit.
#> W_i = (t_i / t_n)^beta should be Uniform(0,1) under H0.
```
