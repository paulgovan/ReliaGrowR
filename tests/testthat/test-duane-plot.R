test_that("duane_plot function works correctly with valid inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane_plot(times, failures)

  expect_s3_class(fit, "lm")  # Check that the function returns a linear model object
  expect_true(length(coef(fit)) == 2)  # Check that the model has two coefficients (intercept and slope)
})

test_that("duane_plot function handles mismatched lengths of times and failures", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1)

  expect_error(duane_plot(times, failures), "The length of 'times' and 'failures' must be equal.")
})

test_that("duane_plot function handles non-positive times", {
  times <- c(-100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  expect_error(duane_plot(times, failures), "All values in 'times' must be greater than 0.")

  times <- c(100, 200, 0, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  expect_error(duane_plot(times, failures), "All values in 'times' must be greater than 0.")
})

test_that("duane_plot function handles non-positive failures", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 0, 1, 3, 2)

  expect_error(duane_plot(times, failures), "Error: All values in 'failures' must be greater than 0.")

  failures <- c(1, -2, 1, 3, 2)

  expect_error(duane_plot(times, failures), "Error: All values in 'failures' must be greater than 0.")
})

