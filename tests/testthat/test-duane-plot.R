# Test setup: example input data
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

# Test: Function works correctly with default parameters
test_that("duane_plot works correctly with default parameters", {
  fit <- duane_plot(times, failures)
  expect_s3_class(fit, "duane")  # Check that the function returns a linear model object
})

# Test: Function works correctly with custom colors and labels
test_that("duane_plot works correctly with custom colors and labels", {
  fit <- duane_plot(times, failures,
                    point_col = "green", line_col = "purple",
                    xlab = "Custom X-axis", ylab = "Custom Y-axis",
                    main = "Custom Title")
  expect_s3_class(fit, "duane")
})

# Test: Function handles mismatched lengths of times and failures
test_that("duane_plot handles mismatched lengths of times and failures", {
  wrong_failures <- c(1, 2, 1)
  expect_error(duane_plot(times, wrong_failures), "The length of 'times' and 'failures' must be equal.")
})

# Test: Function handles non-positive times
test_that("duane_plot handles non-positive times", {
  invalid_times <- c(100, 200, -300, 400, 500)
  expect_error(duane_plot(invalid_times, failures), "All values in 'times' must be greater than 0.")

  invalid_times <- c(100, 200, 0, 400, 500)
  expect_error(duane_plot(invalid_times, failures), "All values in 'times' must be greater than 0.")
})

# Test: Function handles non-positive failures
test_that("duane_plot handles non-positive failures", {
  invalid_failures <- c(1, 0, 1, 3, 2)
  expect_error(duane_plot(times, invalid_failures), "All values in 'failures' must be greater than 0.")

  invalid_failures <- c(1, -2, 1, 3, 2)
  expect_error(duane_plot(times, invalid_failures), "All values in 'failures' must be greater than 0.")
})

# Test: Function works with minimal input
test_that("duane_plot works with minimal input", {
  minimal_times <- c(100, 200)
  minimal_failures <- c(1, 2)
  fit <- duane_plot(minimal_times, minimal_failures)
  expect_s3_class(fit, "duane")
})
