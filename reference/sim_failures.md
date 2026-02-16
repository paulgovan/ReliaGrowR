# Simulate Failures via PPS Sampling

Simulates which units in a non-failed population fail next using
probability proportional to size (PPS) sampling based on unit runtimes.
Units with longer runtimes have a proportionally higher probability of
being selected.

## Usage

``` r
sim_failures(n, runtimes, replace = FALSE)
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

## Value

A data frame with `n` rows sorted by `runtime`, containing:

- index:

  Integer index of the selected unit in `runtimes`.

- runtime:

  Runtime of the selected unit (reported failure time).

## See also

Other data preparation:
[`weibull_to_rga()`](https://paulgovan.github.io/ReliaGrowR/reference/weibull_to_rga.md)

## Examples

``` r
set.seed(42)
runtimes <- c(100, 500, 200, 800, 300)
result <- sim_failures(2, runtimes)
print(result)
#>   index runtime
#> 1     3     200
#> 2     5     300
```
