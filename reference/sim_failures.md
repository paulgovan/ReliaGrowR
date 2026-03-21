# Simulate Failures from a Conditional Weibull Model

Simulates which units in a non-failed population fail next by using a
Weibull life model conditional on each unit's current runtime. When a
positive `window` is supplied, the function calibrates a Weibull scale
parameter (unless provided directly) so that the expected number of
failures within the window matches `n`. Units are then sampled with
probability proportional to their conditional Weibull failure
probability over the window, and failure times are drawn from the
truncated conditional Weibull distribution. The full fleet is returned:
selected units are labelled `"Failure"` and the remaining units are
labelled `"Suspension"`.

## Usage

``` r
sim_failures(n, runtimes, replace = FALSE, window = NULL, beta = 1, eta = NULL)
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
  When provided, failure times are sampled from the conditional Weibull
  distribution over `(runtime, runtime + window]`, and suspension times
  are `runtime + window`.

- beta:

  Positive numeric scalar. Weibull shape parameter used to model the
  age-dependent hazard. Defaults to `1`, corresponding to an
  exponential-process assumption.

- eta:

  `NULL` or a single positive numeric. Weibull scale parameter. When
  `NULL` and `window` is supplied, the scale is calibrated so that the
  expected number of failures across the fleet during the window matches
  `n`.

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

The returned object also carries attributes `weibull_beta` and
`weibull_eta` describing the Weibull parameters used for the simulation.

## Details

When `window = NULL`, the function returns the current fleet state at
the supplied runtimes. In this case, failing units are selected using
relative Weibull hazard weights implied by `beta`.

## See also

Other data preparation:
[`weibull_to_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/weibull_to_rga.md)

## Examples

``` r
set.seed(42)
runtimes <- c(100, 500, 200, 800, 300)
result <- sim_failures(2, runtimes, beta = 1.5)
print(result)
#>   index runtime       type
#> 1     1     100    Failure
#> 2     3     200    Failure
#> 3     5     300 Suspension
#> 4     2     500 Suspension
#> 5     4     800 Suspension

# With an observation window
set.seed(42)
result_w <- sim_failures(2, runtimes, window = 50, beta = 1.5)
print(result_w)
#>   index  runtime       type
#> 1     1 113.8372    Failure
#> 2     3 240.3721    Failure
#> 3     5 350.0000 Suspension
#> 4     2 550.0000 Suspension
#> 5     4 850.0000 Suspension
```
