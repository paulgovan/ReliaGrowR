# Simulate Failures via PPS Sampling

Simulates which units in a non-failed population fail next using
probability proportional to size (PPS) sampling based on unit runtimes.
Units with longer runtimes have a proportionally higher probability of
being selected. The full fleet is returned: selected units are labelled
`"Failure"` and the remaining units are labelled `"Suspension"`.

## Usage

``` r
sim_failures(n, runtimes, replace = FALSE, window = NULL)
```

## Arguments

- n:

  Positive integer. Number of failures to simulate.

- runtimes:

  Numeric vector of positive values. The current operating runtime of
  each unit in the non-failed population.

- replace:

  Logical scalar. If `TRUE`, sampling is done with replacement (a unit
  may be selected more than once). Default is `FALSE`.

- window:

  `NULL` or a single positive numeric. The width of the observation
  window. When `NULL` (default), event times equal current runtimes.
  When provided, failure times are `runtime + Uniform(0, window)` and
  suspension times are `runtime + window`.

## Value

A data frame with `length(runtimes)` rows sorted ascending by `runtime`,
containing:

- index:

  Integer index of the unit in `runtimes`.

- runtime:

  Simulated event time.

- type:

  Character; `"Failure"` for selected units, `"Suspension"` for the
  rest.

## See also

Other data preparation:
[`weibull_to_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/weibull_to_rga.md)

## Examples

``` r
set.seed(42)
runtimes <- c(100, 500, 200, 800, 300)
result <- sim_failures(2, runtimes)
print(result)
#>   index runtime       type
#> 1     1     100 Suspension
#> 2     3     200    Failure
#> 3     5     300    Failure
#> 4     2     500 Suspension
#> 5     4     800 Suspension

# With an observation window
set.seed(42)
result_w <- sim_failures(2, runtimes, window = 50)
print(result_w)
#>   index  runtime       type
#> 1     1 150.0000 Suspension
#> 2     3 214.3070    Failure
#> 3     5 341.5224    Failure
#> 4     2 550.0000 Suspension
#> 5     4 850.0000 Suspension
```
