# Mock data for testing
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
result <- list(
  fitted_values = c(1, 2, 2.5, 4, 5),
  lower_bounds = c(0.8, 1.8, 2.3, 3.8, 4.8),
  upper_bounds = c(1.2, 2.2, 2.7, 4.2, 5.2),
  breakpoints = log(c(200, 400))
)

# Test that plot_rga works with default parameters
test_that("plot_rga works with default parameters", {
  expect_silent(plot_rga(times, failures, result))
})

# Test that plot_rga works with custom colors and labels
test_that("plot_rga works with custom colors and labels", {
  expect_silent(plot_rga(times, failures, result,
                         point_col = "green", line_col = "purple",
                         xlab = "Operating Time", ylab = "Total Failures",
                         main = "Custom Reliability Growth Analysis"))
})

# Test that plot_rga handles mismatched lengths of times and failures
test_that("plot_rga handles mismatched lengths of times and failures", {
  wrong_failures <- c(1, 2, 1)
  expect_error(plot_rga(times, wrong_failures, result), "The length of 'times' and 'failures' must be equal.")
})

# Test that plot_rga handles non-positive times
test_that("plot_rga handles non-positive times", {
  invalid_times <- c(100, 200, -300, 400, 500)
  expect_error(plot_rga(invalid_times, failures, result), "All values in 'times' must be greater than 0.")
})

# Test that plot_rga handles NULL breakpoints in the result
test_that("plot_rga handles NULL breakpoints", {
  no_breakpoints_result <- result
  no_breakpoints_result$breakpoints <- NULL
  expect_silent(plot_rga(times, failures, no_breakpoints_result))
})

# Test that plot_rga works with minimal input
test_that("plot_rga works with minimal input", {
  minimal_times <- c(100, 200)
  minimal_failures <- c(1, 2)
  minimal_result <- list(
    fitted_values = c(1, 2),
    lower_bounds = c(0.9, 1.9),
    upper_bounds = c(1.1, 2.1),
    breakpoints = NULL
  )
  expect_silent(plot_rga(minimal_times, minimal_failures, minimal_result))
})
