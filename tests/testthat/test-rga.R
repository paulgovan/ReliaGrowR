test_that("rga rejects invalid inputs", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 3)

  # length mismatch
  expect_error(rga(times[-1], failures), "length")

  # non-positive values
  expect_error(rga(c(-1, 200), c(1, 2)), "greater")
  expect_error(rga(c(100, 200), c(1, -2)), "greater")

  # invalid conf_level
  expect_error(rga(times, failures, conf_level = -0.1), "between 0 and 1")
  expect_error(rga(times, failures, conf_level = 1), "between 0 and 1")

  # invalid model_type
  expect_error(rga(times, failures, model_type = "invalid"), "Model_type")

  # invalid breaks
  expect_error(rga(times, failures, breaks = c(-10)), "positive")
  expect_error(rga(times, failures, model_type = "Crow-AMSAA", breaks = c(200)), "only be used")
})

test_that("rga runs with Crow-AMSAA model", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  result <- rga(times, failures)

  expect_s3_class(result, "rga")
  expect_true(is.list(result))
  expect_named(result, c("model", "logLik", "AIC", "BIC", "breakpoints",
                         "fitted_values", "lower_bounds", "upper_bounds",
                         "betas", "betas_se", "lambdas"))

  expect_null(result$breakpoints)
  expect_type(result$logLik, "double")
  expect_type(result$AIC, "double")
  expect_true(all(result$fitted_values > 0))
  expect_equal(length(result$fitted_values), length(times))
})

test_that("rga runs with Piecewise NHPP model (auto breakpoint)", {
  skip_if_not_installed("segmented")

  times <- c(100, 200, 300, 400, 500, 600, 700)
  failures <- c(1, 1, 1, 2, 2, 3, 3)

  result <- rga(times, failures, model_type = "Piecewise NHPP")

  expect_s3_class(result, "rga")
  expect_true(!is.null(result$breakpoints))
  expect_true(all(result$betas$log_times[, "Est."] > 0))
})

test_that("print.rga works", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_invisible(out <- print(result))
  expect_s3_class(out, "rga")
})

test_that("plot.rga works with defaults", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_invisible(plot(result, main = "Test Plot"))
})

test_that("plot.rga validates inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  # invalid logical arguments
  expect_error(plot(result, conf_bounds = "yes"), "logical")
  expect_error(plot(result, legend = "yes"), "logical")
  expect_error(plot(result, log = "yes"), "logical")
})

test_that("plot.rga works with options", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_invisible(plot(result, conf_bounds = FALSE, legend = FALSE, log = TRUE))
})
