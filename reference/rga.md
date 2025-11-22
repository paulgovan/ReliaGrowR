# Reliability Growth Analysis.

This function performs reliability growth analysis using the Crow-AMSAA
model by Crow (1975) <https://apps.dtic.mil/sti/citations/ADA020296> or
piecewise NHPP model by Guo et al. (2010)
<doi:10.1109/RAMS.2010.5448029>. It fits a log-log linear regression of
cumulative failures versus cumulative time. The function accepts either
two numeric vectors (`times`, `failures`) or a data frame containing
both. The `Piecewise NHPP` model can automatically detect change points
or use user-specified breakpoints.

## Usage

``` r
rga(
  times,
  failures,
  model_type = "Crow-AMSAA",
  breaks = NULL,
  conf_level = 0.95
)
```

## Arguments

- times:

  Either a numeric vector of cumulative failure times or a data frame
  containing both failure times and failure counts. If a data frame is
  provided, it must contain two columns: `times` and `failures`. The
  `times` column contains cumulative failure times, and the `failures`
  column contains the number of failures at each corresponding time.

- failures:

  A numeric vector of the number of failures at each corresponding time
  in times. Must be the same length as `times` if both are vectors. All
  values must be positive and finite. Ignored if `times` is a data
  frame.

- model_type:

  The model type. Either `Crow-AMSAA` (default) or `Piecewise NHPP` with
  change point detection.

- breaks:

  An optional vector of breakpoints for the `Piecewise NHPP` model.

- conf_level:

  The desired confidence level, which defaults to 95%. The confidence
  level is the probability that the confidence interval contains the
  true mean response.

## Value

The function returns an object of class `rga` that contains:

- times:

  The input cumulative failure times.

- failures:

  The input number of failures.

- n_obs:

  The number of observations (failures).

- cum_failures:

  Cumulative failures.

- model:

  The fitted model object (lm (linear model) or segmented).

- residuals:

  Model residuals on the log-log scale. These represent deviations of
  the observed log cumulative failures from the fitted values and are
  useful for diagnostic checking.

- logLik:

  The log-likelihood of the fitted model. The log-likelihood is a
  measure of model fit, with higher values indicating a better fit.

- AIC:

  Akaike Information Criterion (AIC). AIC is a measure used for model
  selection, with lower values indicating a better fit.

- BIC:

  Bayesian Information Criterion(BIC). BIC is another criterion for
  model selection

- breakpoints:

  Breakpoints (log scale) if applicable.

- fitted_values:

  Fitted cumulative failures on the original scale.

- lower_bounds:

  Lower confidence bounds (original scale).

- upper_bounds:

  Upper confidence bounds (original scale).

- betas:

  Estimated beta(s). Betas are the slopes of the log-log plot.

- betas_se:

  Standard error(s) of the estimated beta(s).

- growth_rate:

  Estimated growth rate(s). Growth rates are calculated as 1 - beta.

- lambdas:

  Estimated lambda(s). Lambdas are the intercepts of the log-log plot.

## Details

The scaling relationship between the size of input data (numbers of
observations) and speed of algorithm execution is approximately linear
(O(n)). The function is efficient and can handle large data sets (e.g.,
thousands of observations) quickly. The function uses the `segmented`
package for piecewise regression, which employs an iterative algorithm
to estimate breakpoints. The number of iterations required for
convergence may vary depending on the data and initial values. In
practice, the function typically converges within a few iterations for
most data sets. However, in some cases, especially with complex data or
poor initial values, it may take more iterations.

## See also

Other Reliability Growth Analysis:
[`plot.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.rga.md),
[`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
result1 <- rga(times, failures)
print(result1)
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

df <- data.frame(times = times, failures = failures)
result2 <- rga(df)
print(result2)
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

result3 <- rga(times, failures, model_type = "Piecewise NHPP")
#> Warning: Breakpoint estimate(s) outdistanced to allow finite estimates and st.errs
print(result3)
#> Reliability Growth Analysis (RGA)
#> ---------------------------------
#> Model Type: Piecewise NHPP 
#> 
#> Breakpoints (original scale):
#> 300.03 
#> 
#> 
#> Number of observations (failures): 5
#> Parameters (per segment):
#>   Growth Rates: 0.0598, 0.2881
#>   Betas: 0.9402, 0.7119
#>   Std. Errors (Betas): 0.2131, 0.255
#>   Lambdas: 0.0132, 0.0484
#> 
#> Goodness of Fit:
#>   Log-likelihood: 5.92
#>   AIC: -1.84
#>   BIC: -3.80

result4 <- rga(times, failures, model_type = "Piecewise NHPP", breaks = c(450))
#> Warning: Breakpoint estimate(s) outdistanced to allow finite estimates and st.errs
print(result4)
#> Reliability Growth Analysis (RGA)
#> ---------------------------------
#> Model Type: Piecewise NHPP 
#> 
#> Breakpoints (original scale):
#> 450 
#> 
#> 
#> Number of observations (failures): 5
#> Parameters (per segment):
#>   Growth Rates: 0.0598, 0.2881
#>   Betas: 0.9402, 0.7119
#>   Std. Errors (Betas): 0.2131, 0.255
#>   Lambdas: 0.0132, 0.0484
#> 
#> Goodness of Fit:
#>   Log-likelihood: 5.92
#>   AIC: -1.84
#>   BIC: -3.80
```
