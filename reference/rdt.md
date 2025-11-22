# Reliability Demonstration Test (RDT) Plan Calculator

This function calculates the required test time or sample size for a
Reliability Demonstration Test (RDT) based on specified reliability,
mission time, confidence level, and Weibull shape parameter.

## Usage

``` r
rdt(target, mission_time, conf_level, beta = 1, n = NULL, test_time = NULL)
```

## Arguments

- target:

  Required reliability at mission time (0 \< target \< 1).

- mission_time:

  Mission duration (time units). Must be greater than 0.

- conf_level:

  Desired confidence level (e.g., 0.9 for 90% confidence). The
  confidence level must be between 0 and 1 (exclusive).

- beta:

  Weibull shape parameter (beta=1 corresponds to exponential
  distribution). Must be greater than 0. Default is 1.

- n:

  Sample size (optional, supply if solving for test_time). Must be a
  positive integer.

- test_time:

  Test time per unit (optional, supply if solving for n). Must be
  greater than 0.

## Value

The function returns an object of class `rdt` that contains:

- Distribution:

  Type of distribution used (Exponential or Weibull).

- Beta:

  Weibull shape parameter.

- Target_Reliability:

  Specified target reliability.

- Mission_Time:

  Specified mission time.

- Required_Test_Time:

  Calculated required test time (if n is provided).

- Input_Sample_Size:

  Provided sample size (if test_time is calculated).

- Required_Sample_Size:

  Calculated required sample size (if test_time is provided).

- Input_Test_Time:

  Provided test time (if n is calculated).

## Examples

``` r
#' # Example 1: Calculate required test time
plan1 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
print(plan1)
#> Reliability Demonstration Test (RDT) Plan
#> -----------------------------------------
#> Distribution:  Exponential 
#> Weibull Shape Parameter (Beta):  1 
#> Target Reliability:  0.9 
#> Mission Time:  1000 
#> Input Sample Size (n):  10 
#> Required Test Time (T):  2185.43 
# Example 2: Calculate required sample size
plan2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, test_time = 2000)
print(plan2)
#> Reliability Demonstration Test (RDT) Plan
#> -----------------------------------------
#> Distribution:  Exponential 
#> Weibull Shape Parameter (Beta):  1 
#> Target Reliability:  0.9 
#> Mission Time:  1000 
#> Input Test Time (T):  2000 
#> Required Sample Size (n):  11 
```
