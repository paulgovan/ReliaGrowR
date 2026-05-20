# Reliability Demonstration Test Planning

``` r

library(ReliaGrowR)
```

## Overview

A **Reliability Demonstration Test (RDT)** provides statistical evidence
that a product meets its reliability requirement. The
[`rdt()`](https://paulgovan.github.io/ReliaGrowR/reference/rdt.md)
function computes either:

- the **required test time** given a fixed sample size, or
- the **required sample size** given a fixed test duration.

The calculation uses the Weibull distribution; setting `beta = 1` (the
default) gives the exponential special case.

## Test Time

For a zero-failure test plan (`f = 0`), the required cumulative test
time $`T`$ for $`n`$ units satisfying reliability $`R`$ at mission time
$`t_m`$ with confidence $`C`$ is:

``` math
T = t_m \left(\frac{-\ln C}{-\ln R}\right)^{1/\beta}
```

When allowable failures $`f > 0`$, the chi-squared quantile replaces
$`-\ln C`$:

``` math
T = \frac{t_m}{(-\ln R)^{1/\beta}} \cdot \left(\frac{\chi^2_{1-C,\,2(f+1)}}{2n}\right)^{1/\beta}
```

## Solving for Test Time

Suppose you need to demonstrate 90% reliability at 500 hours with 90%
confidence, and you have 20 units available for testing:

``` r

plan <- rdt(
  target       = 0.90,   # 90% reliability
  mission_time = 500,    # hours
  conf_level   = 0.90,   # 90% confidence
  n            = 20      # 20 test units
)
print(plan)
#> Reliability Demonstration Test (RDT) Plan
#> -----------------------------------------
#> Distribution:  Exponential 
#> Weibull Shape Parameter (Beta):  1 
#> Allowed Failures (f):  0 
#> Target Reliability:  0.9 
#> Mission Time:  500 
#> Input Sample Size (n):  20 
#> Required Test Time (T):  546.36
```

Each unit must be tested for `Required_Test_Time` hours (or failure,
whichever comes first) with zero failures allowed.

## Solving for Sample Size

Alternatively, fix the test duration at 800 hours per unit:

``` r

plan2 <- rdt(
  target       = 0.90,
  mission_time = 500,
  conf_level   = 0.90,
  test_time    = 800
)
print(plan2)
#> Reliability Demonstration Test (RDT) Plan
#> -----------------------------------------
#> Distribution:  Exponential 
#> Weibull Shape Parameter (Beta):  1 
#> Allowed Failures (f):  0 
#> Target Reliability:  0.9 
#> Mission Time:  500 
#> Input Test Time (T):  800 
#> Required Sample Size (n):  14
```

## Effect of Weibull Shape

For products with wear-out (`beta > 1`) or infant mortality
(`beta < 1`), the Weibull shape parameter changes the required test
time. Here we compare three scenarios:

``` r

betas <- c(0.8, 1.0, 1.5)
for (b in betas) {
  p <- rdt(target = 0.90, mission_time = 500, conf_level = 0.90, n = 20, beta = b)
  cat(sprintf("beta = %.1f  ->  required test time = %.1f hours\n",
              b, p$Required_Test_Time))
}
#> beta = 0.8  ->  required test time = 558.6 hours
#> beta = 1.0  ->  required test time = 546.4 hours
#> beta = 1.5  ->  required test time = 530.4 hours
```

Higher beta (wear-out) demands shorter tests because failures
concentrate near end-of-life; lower beta (infant mortality) demands
longer tests.

## Allowing Failures

A zero-failure plan is conservative. Allowing a small number of failures
can substantially reduce the test burden:

``` r

for (f in 0:3) {
  p <- rdt(target = 0.90, mission_time = 500, conf_level = 0.90, n = 20, f = f)
  cat(sprintf("f = %d  ->  required test time = %.1f hours\n",
              f, p$Required_Test_Time))
}
#> f = 0  ->  required test time = 546.4 hours
#> f = 1  ->  required test time = 923.0 hours
#> f = 2  ->  required test time = 1262.9 hours
#> f = 3  ->  required test time = 1585.2 hours
```

## Summary

| Parameter      | Role                                            |
|----------------|-------------------------------------------------|
| `target`       | Required reliability R(t_m)                     |
| `mission_time` | The time at which reliability is evaluated      |
| `conf_level`   | Statistical confidence in the demonstration     |
| `beta`         | Weibull shape (1 = exponential)                 |
| `f`            | Allowable failures (0 = most conservative)      |
| `n`            | Sample size → solves for test time              |
| `test_time`    | Test duration per unit → solves for sample size |
