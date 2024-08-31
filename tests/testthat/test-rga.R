
# Tests for the rga function
test_that("rga function works correctly with valid inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures, model_type = "Crow-AMSAA", conf_level = 0.95)

  expect_true(is.list(result))
  expect_true(all(c("model", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds") %in% names(result)))
  expect_true(is.null(result$breakpoints))  # No change points for Crow-AMSAA
})

test_that("rga function works correctly with Piecewise Weibull NHPP model", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures, model_type = "Piecewise Weibull NHPP", conf_level = 0.95)

  expect_true(is.list(result))
  expect_true(all(c("model", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds") %in% names(result)))
  expect_false(is.null(result$breakpoints))  # Change points should be detected
})

test_that("rga function handles invalid model_type", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  expect_error(rga(times, failures, model_type = "InvalidModelType"), "Error: model_type must be one of Crow-AMSAA, Piecewise Weibull NHPP.")
})

# test_that("rga function handles invalid conf_level", {
#   times <- c(100, 200, 300, 400, 500)
#   failures <- c(1, 2, 1, 3, 2)
#
#   expect_error(rga(times, failures, conf_level = 1.5), "Error: conf_level must be a numeric value between 0 and 1 (exclusive).")
#   expect_error(rga(times, failures, conf_level = -0.5), "Error: conf_level must be a numeric value between 0 and 1 (exclusive).")
# })

test_that("rga function handles mismatched lengths of times and failures", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1)

  expect_error(rga(times, failures), "The length of 'times' and 'failures' must be equal.")
})

# test_that("rga function handles non-positive times", {
#   times <- c(-100, 200, 300, 400, 500)
#   failures <- c(1, 2, 1, 3, 2)
#
#   expect_error(rga(times, failures), "Error: All values in 'times' must be greater than 0.")
# })
