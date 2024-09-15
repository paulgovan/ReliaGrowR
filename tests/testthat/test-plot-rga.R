# Mock data for testing
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

# Test that plot_rga works with default parameters
test_that("plot_rga works with default parameters", {
  result <- rga(times, failures)  # Ensure the rga object is correctly created
  expect_silent(plot_rga(result))
})

# Test that plot_rga works with custom colors and labels
test_that("plot_rga works with custom colors and labels", {
  result <- rga(times, failures)
  expect_silent(plot_rga(result,
                         point_col = "green", line_col = "purple",
                         xlab = "Operating Time", ylab = "Total Failures",
                         main = "Custom Reliability Growth Analysis"))
})

# Test that rga handles mismatched lengths of times and failures
test_that("rga handles mismatched lengths of times and failures", {
  wrong_failures <- c(1, 2, 1)  # Incorrect length for failures
  expect_error(rga(times, wrong_failures), "The length of 'times' and 'failures' must be equal.")
})

# Test that rga handles non-positive times
test_that("rga handles non-positive times", {
  invalid_times <- c(100, 200, -300, 400, 500)  # Non-positive value
  expect_error(rga(invalid_times, failures), "All values in 'times' must be greater than 0.")
})

# Test that plot_rga handles NULL breakpoints in the result
test_that("plot_rga handles NULL breakpoints", {
  result <- rga(times, failures, breakpoints = NULL)  # No breakpoints
  expect_silent(plot_rga(result))
})

# Test that plot_rga works with minimal input
test_that("plot_rga works with minimal input", {
  minimal_times <- c(100, 200)
  minimal_failures <- c(1, 2)
  result <- rga(minimal_times, minimal_failures)
  expect_silent(plot_rga(result))
})
