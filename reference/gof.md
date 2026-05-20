# Goodness-of-Fit Statistics for RGA Objects

Computes numerical goodness-of-fit statistics for a fitted Reliability
Growth Analysis (RGA) model using the time-transformation approach. For
a Crow-AMSAA (Power Law NHPP) model with parameters \\\beta\\ and
\\\lambda\\, the transformed values \\W_i = (t_i / t_n)^{\beta}\\ should
follow a Uniform(0, 1) distribution if the model fits. The Cramér-von
Mises and Kolmogorov-Smirnov statistics are computed against this null
distribution.

## Usage

``` r
gof(x, ...)

# Default S3 method
gof(x, ...)

# S3 method for class 'rga'
gof(x, ...)
```

## Arguments

- x:

  An object for which a goodness-of-fit method is defined.

- ...:

  Additional arguments passed to methods.

## Value

An object of class `gof`.

## See also

Other goodness-of-fit:
[`ppplot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/ppplot.rga.md),
[`print.gof()`](https://paulgovan.github.io/ReliaGrowR/reference/print.gof.md),
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
