# Plot Method for RGA Objects

This function generates plots for objects of class `rga`.

## Usage

``` r
# S3 method for class 'rga'
plot(
  x,
  conf_bounds = TRUE,
  legend = TRUE,
  log = FALSE,
  legend_pos = "bottomright",
  ...
)
```

## Arguments

- x:

  An object of class `rga`, which contains the results from the RGA
  model.

- conf_bounds:

  Logical; include confidence bounds (default: TRUE).

- legend:

  Logical; show the legend (default: TRUE).

- log:

  Logical; use a log-log scale (default: FALSE).

- legend_pos:

  Position of the legend (default: "bottomright").

- ...:

  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly returns `NULL`.

## See also

Other Reliability Growth Analysis:
[`print.rga()`](https://paulgovan.github.io/ReliaGrowR/reference/print.rga.md),
[`rga()`](https://paulgovan.github.io/ReliaGrowR/reference/rga.md)

## Examples

``` r
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
result <- rga(times, failures)
plot(result,
  main = "Reliability Growth Analysis",
  xlab = "Cumulative Time", ylab = "Cumulative Failures"
)
```
