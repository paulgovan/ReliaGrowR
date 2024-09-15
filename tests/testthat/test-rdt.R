# Test valid input for the Exponential Distribution with a fixed sample size
test_that("rdt works correctly for Exponential Distribution with fixed sample size", {
  result <- rdt(
    sample_size = 100,
    target = 0.95,
    conf_level = 0.90,
    failure_rate = 0.01,
    dist_type = "exponential"
  )

  expect_true(is.list(result))
  expect_named(result, c("Sample_Size", "Test_Time", "target", "conf_level", "Failure_Rate", "Distribution_Type", "shape", "scale"))
  expect_equal(result$Distribution_Type, "exponential")
})

# Test valid input for the Weibull Distribution with fixed test time
test_that("rdt works correctly for Weibull Distribution with fixed test time", {
  result <- rdt(
    test_time = 500,
    target = 0.95,
    conf_level = 0.90,
    dist_type = "Weibull",
    shape = 1.5,
    scale = 200
  )

  expect_true(is.list(result))
  expect_named(result, c("Sample_Size", "Test_Time", "target", "conf_level", "Failure_Rate", "Distribution_Type", "shape", "scale"))
  expect_equal(result$Distribution_Type, "Weibull")
})

# Test valid input for the Lognormal Distribution
test_that("rdt works correctly for Lognormal Distribution", {
  result <- rdt(
    sample_size = 100,
    target = 0.95,
    conf_level = 0.90,
    dist_type = "lognormal",
    shape = 0.5,
    scale = 5
  )

  expect_true(is.list(result))
  expect_equal(result$Distribution_Type, "lognormal")
})

# Test error when missing required parameters for Exponential Distribution
test_that("rdt throws an error when failure_rate is missing for Exponential Distribution", {
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = 0.90, dist_type = "exponential"),
               "Failure rate is required for the exponential distribution.")
})

# Test error when missing shape and scale for Weibull Distribution
test_that("rdt throws an error when shape or scale is missing for Weibull Distribution", {
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = 0.90, dist_type = "Weibull"),
               "Both shape and scale parameters are required for the Weibull distribution.")
})

# Test error when missing shape and scale for Lognormal Distribution
test_that("rdt throws an error when shape or scale is missing for Lognormal Distribution", {
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = 0.90, dist_type = "lognormal"),
               "Both shape and scale parameters are required for the lognormal distribution.")
})

# Test error when conf_level is invalid
test_that("rdt throws an error when conf_level is out of bounds", {
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = 1.5, failure_rate = 0.01, dist_type = "exponential"),
               "Confidence level should be between 0 and 1.")
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = -0.5, failure_rate = 0.01, dist_type = "exponential"),
               "Confidence level should be between 0 and 1.")
})

# Test error when target reliability is invalid
test_that("rdt throws an error when target reliability is out of bounds", {
  expect_error(rdt(sample_size = 100, target = 1.5, conf_level = 0.90, failure_rate = 0.01, dist_type = "exponential"),
               "Target reliability should be between 0 and 1.")
  expect_error(rdt(sample_size = 100, target = -0.5, conf_level = 0.90, failure_rate = 0.01, dist_type = "exponential"),
               "Target reliability should be between 0 and 1.")
})

# Test error when unsupported distribution type is used
test_that("rdt throws an error for unsupported distribution type", {
  expect_error(rdt(sample_size = 100, target = 0.95, conf_level = 0.90, dist_type = "unsupported"),
               "Unsupported distribution type. Choose 'exponential', 'Weibull', or 'lognormal'.")
})

# Test that rdt works correctly with minimal input (for Exponential Distribution)
test_that("rdt works with minimal input for Exponential Distribution", {
  result <- rdt(
    sample_size = 50,
    target = 0.9,
    conf_level = 0.95,
    failure_rate = 0.02,
    dist_type = "exponential"
  )

  expect_true(is.list(result))
  expect_named(result, c("Sample_Size", "Test_Time", "target", "conf_level", "Failure_Rate", "Distribution_Type", "shape", "scale"))
})

