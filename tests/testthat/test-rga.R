test_that("rga throws error for mismatched input lengths", {
  expect_error(rga(c(100, 200), c(1)), "length of 'times' and 'failures'")
})

test_that("rga throws error for non-positive times", {
  expect_error(rga(c(-100, 200), c(1, 2)), "must be greater than 0")
})

test_that("rga throws error for non-positive failures", {
  expect_error(rga(c(100, 200), c(1, 0)), "must be greater than 0")
})

test_that("rga throws error for invalid conf_level", {
  expect_error(rga(c(100, 200), c(1, 2), conf_level = 1.5), "Conf_level must be a numeric value")
})

test_that("rga throws error for invalid model_type", {
  expect_error(rga(c(100, 200), c(1, 2), model_type = "Invalid"), "Model_type must be one of")
})

test_that("rga throws error when breaks are used with invalid model", {
  expect_error(rga(c(100, 200), c(1, 2), model_type = "Crow-AMSAA", breaks = c(150)), "Breakpoints can only be used")
})

test_that("rga throws error for invalid breaks input", {
  expect_error(rga(c(100, 200), c(1, 2), model_type = "Piecewise NHPP", breaks = c(-150)), "Breakpoints must be a numeric vector")
})

test_that("rga returns an object of class 'rga'", {
  result <- rga(c(100, 200, 300), c(1, 2, 1))
  expect_s3_class(result, "rga")
})

test_that("Crow-AMSAA model returns expected components", {
  result <- rga(c(100, 200, 300), c(1, 2, 1))
  expect_named(result, c("model", "AIC", "BIC", "breakpoints", "fitted_values",
                         "lower_bounds", "upper_bounds", "betas", "lambdas"))
  expect_null(result$breakpoints)
})

test_that("Piecewise NHPP model handles automatic segmentation", {
  result <- rga(c(100, 200, 300, 400, 500, 600), c(1, 1, 2, 2, 3, 2), model_type = "Piecewise NHPP")
  expect_s3_class(result, "rga")
  expect_true(!is.null(result$breakpoints))
})

test_that("Print method for rga executes without error", {
  result <- rga(c(100, 200, 300), c(1, 2, 1))
  expect_output(print(result), "Reliability Growth Analysis")
})
