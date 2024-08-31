test_that("plot_rga function works correctly with valid inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_silent(plot_rga(times, failures, result))
})

test_that("plot_rga function handles non-numeric times or failures", {
  times <- c("a", "b", "c", "d", "e")
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))

  expect_error(plot_rga(times, failures, result), "Error: Both 'times' and 'failures' must be numeric vectors.")

  times <- c(100, 200, 300, 400, 500)
  failures <- c("a", "b", "c", "d", "e")

  expect_error(plot_rga(times, failures, result), "Error: Both 'times' and 'failures' must be numeric vectors.")
})

test_that("plot_rga function handles mismatched lengths of times and failures", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1)
  result <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))

  expect_error(plot_rga(times, failures, result), "Error: The length of 'times' and 'failures' must be equal.")
})

test_that("plot_rga function handles non-positive times", {
  times <- c(-100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))

  expect_error(plot_rga(times, failures, result), "Error: All values in 'times' must be greater than 0.")
})

test_that("plot_rga function handles invalid result list", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  invalid_result <- list(fitted_values = c(1, 2, 3, 4, 5))

  expect_error(plot_rga(times, failures, invalid_result), "Error: 'result' must be a list containing 'fitted_values', 'lower_bounds', and 'upper_bounds'.")
})

