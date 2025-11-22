# Duane Analysis

This function performs a Duane analysis (1962)
<doi:10.1109/TA.1964.4319640> on failure data by fitting a log-log
linear regression of cumulative Mean Time Between Failures (MTBF) versus
cumulative time. The function accepts either two numeric vectors
(`times`, `failures`) or a data frame containing both.

## Usage

``` r
duane(times, failures = NULL, conf.level = 0.95)
```

## Arguments

- times:

  Either:

  - A numeric vector of cumulative failure times, or

  - A data frame containing two columns: `times` and `failures`. The
    `times` column contains cumulative failure times, and the `failures`
    column contains the number of failures at each corresponding time.

- failures:

  A numeric vector of the number of failures at each corresponding time
  in `times`. Ignored if `times` is a data frame. Must be the same
  length as `times` if both are vectors. All values must be positive and
  finite.

- conf.level:

  Confidence level for the confidence bounds (default: `0.95`). Must be
  between 0 and 1 (exclusive).

## Value

A list of class `"duane"` containing:

- times:

  The input cumulative failure times.

- failures:

  The input number of failures.

- n_obs:

  The number of observations (failures).

- MTBF:

  The cumulative mean time between failures.

- model:

  The fitted `lm` (linear model) object containing the regression
  results.

- logLik:

  The log-likelihood of the fitted model.

- AIC:

  Akaike Information Criterion (AIC).

- BIC:

  Bayesian Information Criterion (BIC).

- conf.level:

  The confidence level.

- Cumulative_Time:

  The cumulative operating times.

- Cumulative_MTBF:

  The cumulative mean time between failures.

- Fitted_Values:

  The fitted values on the MTBF scale.

- Confidence_Bounds:

  Matrix of fitted values and confidence bounds on the MTBF scale.

- Residuals_Log:

  Residuals on the log(MTBF) scale (from the regression).

- Residuals_MTBF:

  Residuals on the MTBF scale (observed - fitted).

## Details

The scaling relationship between the size of input data (numbers of
observations) and speed of algorithm execution is approximately linear
(O(n)). The function is efficient and can handle large data sets (e.g.,
thousands of observations) quickly. The function uses base R functions
and does not require any additional packages. The function includes
comprehensive input validation and error handling to ensure robustness.
The function is tested with a standard data set from a published paper
and includes unit tests to verify correctness and performance.

## See also

Other Duane functions:
[`plot.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/plot.duane.md),
[`print.duane()`](https://paulgovan.github.io/ReliaGrowR/reference/print.duane.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
fit1 <- duane(times, failures, conf.level = 0.90)
print(fit1)
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
#> Confidence level: 90.0%

df <- data.frame(times = times, failures = failures)
fit2 <- duane(df, conf.level = 0.95)
print(fit2)
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
