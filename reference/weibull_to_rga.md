# Weibull to RGA

Converts Weibull data (failure, suspension, and interval-censored times)
into a format suitable for reliability growth analysis (RGA). The
function handles exact failure times, right-censored suspensions, and
interval-censored data. It approximates interval-censored failures by
placing them at the midpoint of the interval. The output is a data frame
with cumulative time and failure counts. This format can be used with
RGA models such as Crow-AMSAA.

## Usage

``` r
weibull_to_rga(
  failures,
  suspensions = NULL,
  interval_starts = NULL,
  interval_ends = NULL
)
```

## Arguments

- failures:

  A numeric vector of exact failure times. Each failure time indicates
  when an item failed during the observation period.

- suspensions:

  A numeric vector of suspension (right-censored) times. A suspension
  indicates that the item was removed from observation at that time
  without failure. This parameter is optional and can be NULL if there
  are no suspensions.

- interval_starts:

  A numeric vector of interval start times (lower bound of censoring).
  This parameter is optional and can be NULL if there are no
  interval-censored data. If provided, it must be the same length as
  `interval_ends`.

- interval_ends:

  A numeric vector of interval end times (upper bound of censoring).
  This parameter is optional and can be NULL if there are no
  interval-censored data. If provided, it must be the same length as
  `interval_starts`.

## Value

The data frame contains two columns:

- CumulativeTime:

  Cumulative time at each failure event.

- Failures:

  Number of failures at each cumulative time point.

The function approximates interval-censored failures by placing them at
the midpoint of the interval.

## Examples

``` r
failures <- c(100, 200, 200, 400)
suspensions <- c(250, 350, 450)
interval_starts <- c(150, 300)
interval_ends <- c(180, 320)
result <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)
print(result)
#>   CumulativeTime Failures
#> 1            100        1
#> 2            265        1
#> 3            465        2
#> 6           1225        1
#> 8           1975        1
```
