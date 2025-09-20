test_that("duane rejects invalid inputs", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 3)

  # length mismatch
  expect_error(duane(times[-1], failures), "length")

  # non-positive times/failures
  expect_error(duane(c(-1, 200), c(1, 2)), "> 0")
  expect_error(duane(c(100, 200), c(1, -2)), "> 0")
})

test_that("duane runs and always returns confidence intervals", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane(times, failures)

  expect_s3_class(fit, "duane")
  expect_named(fit, c("model", "logLik", "AIC", "BIC", "conf.level",
                      "Cumulative_Time", "Cumulative_MTBF",
                      "Fitted_Values", "Confidence_Bounds"))

  # confidence intervals are always present
  expect_equal(fit$conf.level, 0.95)
  expect_true(!is.null(fit$Confidence_Bounds))
  expect_equal(nrow(fit$Confidence_Bounds), length(times))
  expect_true(all(fit$Confidence_Bounds[, "lwr"] > 0))
  expect_true(all(fit$Confidence_Bounds[, "upr"] > 0))

  expect_true(all(fit$Cumulative_Time > 0))
  expect_true(all(fit$Cumulative_MTBF > 0))
  expect_equal(length(fit$Fitted_Values), length(times))
})

test_that("duane runs with custom confidence level", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane(times, failures, conf.level = 0.90)

  expect_s3_class(fit, "duane")
  expect_equal(fit$conf.level, 0.90)
  expect_true(!is.null(fit$Confidence_Bounds))
  expect_equal(nrow(fit$Confidence_Bounds), length(times))
  expect_true(all(fit$Confidence_Bounds[, "lwr"] > 0))
  expect_true(all(fit$Confidence_Bounds[, "upr"] > 0))
})

test_that("print.duane works", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane(times, failures)
  expect_invisible(out <- print(fit))
  expect_s3_class(out, "duane")
})

test_that("plot.duane works with defaults", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane(times, failures)
  expect_invisible(plot(fit, main = "Duane Plot"))
})

test_that("plot.duane works with options", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit <- duane(times, failures)

  # linear scale, no CI plotting, no legend
  expect_invisible(plot(fit, log = FALSE,
                        conf.int = FALSE,
                        legend = FALSE))
})
