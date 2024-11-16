# Mock data for testing
valid_times <- c(100, 200, 300, 400, 500)
valid_failures <- c(1, 2, 1, 3, 2)

# Test that rga works correctly with valid inputs (Crow-AMSAA)
test_that("rga works correctly with valid inputs for Crow-AMSAA", {
  result <- rga(valid_times, valid_failures, model_type = "Crow-AMSAA", conf_level = 0.95)

  expect_true(is.list(result))
  expect_named(result, c("model", "AIC", "BIC", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds", "betas", "lambdas"))
  expect_null(result$breakpoints)  # Crow-AMSAA model doesn't have breakpoints
})

# Test that rga works correctly with valid inputs for Piecewise Weibull NHPP
test_that("rga works correctly with valid inputs for Piecewise Weibull NHPP", {
  result <- rga(valid_times, valid_failures, model_type = "Piecewise Weibull NHPP", conf_level = 0.95)

  expect_true(is.list(result))
  expect_named(result, c("model", "AIC", "BIC", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds", "betas", "lambdas"))
  expect_false(is.null(result$breakpoints))  # Piecewise Weibull NHPP should detect breakpoints
})

# Test that rga handles invalid model_type
test_that("rga handles invalid model_type", {
  expect_error(rga(valid_times, valid_failures, model_type = "InvalidModelType"), "Model_type must be one of Crow-AMSAA, Piecewise Weibull NHPP.")
})

# # Test that rga handles invalid confidence levels
# test_that("rga handles invalid conf_level", {
#   expect_error(rga(valid_times, valid_failures, conf_level = 1.5), "Conf_level must be a numeric value between 0 and 1 (exclusive).")
#   expect_error(rga(valid_times, valid_failures, conf_level = -0.5), "Conf_level must be a numeric value between 0 and 1 (exclusive).")
# })

# Test that rga handles mismatched lengths of times and failures
test_that("rga handles mismatched lengths of times and failures", {
  mismatched_failures <- c(1, 2, 1)  # Mismatched failure vector length
  expect_error(rga(valid_times, mismatched_failures), "The length of 'times' and 'failures' must be equal.")
})

# Test that rga handles non-positive times
test_that("rga handles non-positive times", {
  invalid_times <- c(-100, 200, 300, 400, 500)  # Non-positive value in times
  expect_error(rga(invalid_times, valid_failures), "All values in 'times' must be greater than 0.")
})

# Test that rga works correctly with valid user-supplied breakpoints
# test_that("rga works correctly with valid user-supplied breakpoints", {
#   breakpoints <- c(300)
#   result <- rga(valid_times, valid_failures, model_type = "Piecewise Weibull NHPP", breakpoints = breakpoints)
#
#   expect_true(is.list(result))
#   expect_named(result, c("model", "AIC", "BIC", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds", "shape_parameters", "scale_parameters", "betas", "lambdas"))
#   expect_equal(length(result$breakpoints), length(breakpoints))  # Number of breakpoints should match
# })

# Test that rga handles invalid breakpoints
test_that("rga handles invalid breakpoints", {
  invalid_breakpoints <- c(-150, 350)  # Invalid breakpoints
  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise Weibull NHPP", breakpoints = invalid_breakpoints), "Breakpoints must be a numeric vector with positive values.")
  expect_error(rga(valid_times, valid_failures, model_type = "Crow-AMSAA", breakpoints = c(150, 350)), "Breakpoints can only be used with the 'Piecewise Weibull NHPP' model.")
})

# Test that rga works without user-supplied breakpoints (should auto-detect breakpoints)
test_that("rga works without user-supplied breakpoints", {
  result <- rga(valid_times, valid_failures, model_type = "Piecewise Weibull NHPP")

  expect_true(is.list(result))
  expect_false(is.null(result$breakpoints))  # Breakpoints should be auto-detected
})

# Test that rga works with minimal input (Crow-AMSAA model)
test_that("rga works with minimal input", {
  minimal_times <- c(100, 200)
  minimal_failures <- c(1, 2)
  result <- rga(minimal_times, minimal_failures, model_type = "Crow-AMSAA")

  expect_true(is.list(result))
  expect_named(result, c("model", "AIC", "BIC", "breakpoints", "fitted_values", "lower_bounds", "upper_bounds", "betas", "lambdas"))
  expect_null(result$breakpoints)  # Crow-AMSAA doesn't have breakpoints
})
