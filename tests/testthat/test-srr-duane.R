set.seed(123)

test_that("data.frame input works the same as separate vectors", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 1)
  df <- data.frame(times = times, failures = failures)

  res_df <- duane(df)
  res_vec <- duane(times, failures)

  expect_s3_class(res_df, "duane")
  expect_s3_class(res_vec, "duane")
  expect_equal(res_df$Cumulative_Time, res_vec$Cumulative_Time)
  expect_equal(res_df$Cumulative_MTBF, res_vec$Cumulative_MTBF)
  expect_equal(res_df$Fitted_Values, res_vec$Fitted_Values)
})

test_that("data.frame without required columns errors", {
  df1 <- data.frame(x = 1:3, y = 1:3)
  expect_error(duane(df1),
               "must contain columns named 'times' and 'failures'")

  df2 <- data.frame(times = 1:3)
  expect_error(duane(df2),
               "must contain columns named 'times' and 'failures'")
})

test_that("NA or NaN in data.frame input throws error", {
  df_na <- data.frame(times = c(100, 200, NA), failures = c(1, 2, 1))
  df_nan <- data.frame(times = c(100, NaN, 300), failures = c(1, 2, 1))
  df_fail_na <- data.frame(times = c(100, 200, 300), failures = c(1, NA, 1))

  expect_error(duane(df_na), "'times' contains missing")
  expect_error(duane(df_nan), "'times' contains missing")
  expect_error(duane(df_fail_na), "'failures' contains missing")
})

test_that("data.frame with non-positive or infinite values errors", {
  df_zero_time <- data.frame(times = c(0, 100, 200), failures = c(1, 2, 1))
  df_neg_fail <- data.frame(times = c(100, 200, 300), failures = c(1, -1, 1))
  df_inf_time <- data.frame(times = c(100, Inf, 200), failures = c(1, 2, 1))

  expect_error(duane(df_zero_time), "must be finite and > 0")
  expect_error(duane(df_neg_fail), "must be finite and > 0")
  expect_error(duane(df_inf_time), "must be finite and > 0")
})

test_that("data.frame input respects conf.level parameter", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 1)
  df <- data.frame(times = times, failures = failures)

  res <- duane(df, conf.level = 0.90)
  expect_equal(res$conf.level, 0.90)
})

# Helper: minimal valid inputs
valid_times <- c(100, 200, 300)
valid_failures <- c(1, 2, 1)

test_that("data frame without required columns errors", {
  df_bad <- data.frame(a = 1:3, b = 1:3)
  expect_error(duane(df_bad),
               "Data frame input must contain columns named 'times' and 'failures'.",
               fixed = TRUE)
})

test_that("NA / NaN checks for times and failures", {
  expect_error(duane(c(1, NA), valid_failures),
               "'times' contains missing (NA) or NaN values.",
               fixed = TRUE)

  expect_error(duane(valid_times, c(1, NaN, 1)),
               "'failures' contains missing (NA) or NaN values.",
               fixed = TRUE)
})

test_that("type checks for times and failures", {
  expect_error(duane(list(1,2,3), valid_failures),
               "'times' must be a numeric vector.",
               fixed = TRUE)

  expect_error(duane(valid_times, list(1,2,3)),
               "'failures' must be a numeric vector.",
               fixed = TRUE)
})

test_that("empty vector checks", {
  expect_error(duane(numeric(0), numeric(0)),
               "'times' cannot be empty.",
               fixed = TRUE)

  expect_error(duane(valid_times, numeric(0)),
               "'failures' cannot be empty.",
               fixed = TRUE)
})

test_that("length mismatch check", {
  expect_error(duane(c(1, 2), c(1, 2, 3)),
               "The length of 'times' and 'failures' must be equal.",
               fixed = TRUE)
})

test_that("finite and >0 checks for times and failures", {
  expect_error(duane(c(1, Inf, 3), c(1,1,1)),
               "All values in 'times' must be finite and > 0.",
               fixed = TRUE)

  expect_error(duane(valid_times, c(1, 0, 2)),
               "All values in 'failures' must be finite and > 0.",
               fixed = TRUE)
})

test_that("conf.level validation", {
  expect_error(duane(valid_times, valid_failures, conf.level = c(0.9, 0.95)),
               "'conf.level' must be a single numeric value.",
               fixed = TRUE)

  expect_error(duane(valid_times, valid_failures, conf.level = 1),
               "'conf.level' must be between 0 and 1 (exclusive).",
               fixed = TRUE)

  expect_error(duane(valid_times, valid_failures, conf.level = 0),
               "'conf.level' must be between 0 and 1 (exclusive).",
               fixed = TRUE)
})

test_that("print.duane errors when input is not duane", {
  expect_error(print.duane(list()),
               "'x' must be an object of class 'duane'.",
               fixed = TRUE)
})

test_that("plot.duane argument type checks", {
  # Minimal valid duane object
  dummy_fit <- duane(valid_times, valid_failures)

  # inherits check
  expect_error(plot.duane(list()),
               "'x' must be an object of class 'duane'.",
               fixed = TRUE)

  # log must be single logical
  expect_error(plot.duane(dummy_fit, log = "nope"),
               "'log' must be a single logical value.",
               fixed = TRUE)

  # conf.int must be single logical
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = "nope"),
               "'conf.int' must be a single logical value.",
               fixed = TRUE)

  # legend must be single logical
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = TRUE, legend = "nope"),
               "'legend' must be a single logical value.",
               fixed = TRUE)

  # legend.pos must be single character
  expect_error(plot.duane(dummy_fit, log = TRUE, conf.int = TRUE, legend = TRUE, legend.pos = c("a","b")),
               "'legend.pos' must be a single character string.",
               fixed = TRUE)
})

test_that("duane recovers known parameters (linear log-log relationship)", {
  # Generate synthetic data for constant MTBF process
  times <- rep(100, 10)  # equal spacing of 100
  failures <- rep(1, 10) # one failure per interval

  fit <- duane(times, failures, conf.level = 0.95)

  coef_est <- coef(fit$model)

  # Expected: slope ~ 1, intercept ~ log(constant)
  expect_equal(unname(exp(coef_est[2])), 1, tolerance = 0.2)
})

test_that("duane returns same result for vector and data.frame inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit_vec <- duane(times, failures)
  fit_df  <- duane(data.frame(times = times, failures = failures))

  expect_equal(fit_vec$Cumulative_MTBF,
               fit_df$Cumulative_MTBF,
               tolerance = 1e-12)

  expect_equal(coef(fit_vec$model),
               coef(fit_df$model),
               tolerance = 1e-12)
})

test_that("confidence level is respected", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)

  fit90 <- duane(times, failures, conf.level = 0.90)
  fit95 <- duane(times, failures, conf.level = 0.95)

  # Wider interval for higher confidence level
  width90 <- mean(fit90$Confidence_Bounds[, "upr"] - fit90$Confidence_Bounds[, "lwr"])
  width95 <- mean(fit95$Confidence_Bounds[, "upr"] - fit95$Confidence_Bounds[, "lwr"])

  expect_gt(width95, width90)
})

test_that("slope approaches 1 for constant MTBF process", {
  # Failures occur at constant intervals
  times <- rep(100, 20)
  failures <- rep(1, 20)

  fit <- duane(times, failures)
  slope <- unname(exp(coef(fit$model)[2]))

  expect_equal(slope, 1, tolerance = 0.2)
})

test_that("slope is less than 1 for reliability growth process", {
  # Failures get further apart over time (improving reliability)
  times <- seq(100, 1000, length.out = 20)
  failures <- rep(1, 20)

  fit <- duane(times, failures)
  slope <- coef(fit$model)[2]

  expect_lt(slope, 1)
})

test_that("cumulative MTBF increases with cumulative time if failures constant", {
  times <- rep(100, 10)
  failures <- rep(1, 10)

  fit <- duane(times, failures)

  expect_true(all(diff(fit$Cumulative_MTBF) >= 0))
})

# test_that("loglik increases with more data", {
#   times_small <- rep(100, 5)
#   failures_small <- rep(1, 5)
#   fit_small <- duane(times_small, failures_small)
#
#   times_large <- rep(100, 50)
#   failures_large <- rep(1, 50)
#   fit_large <- duane(times_large, failures_large)
#
#   # Larger datasets should yield smaller logLik but larger penalties
#   expect_lt(fit_small$logLik, fit_large$logLik) # logLik increases with more data
#
# })

test_that("adding small noise to times does not change slope significantly", {
  times <- rep(100, 20)
  failures <- rep(1, 20)

  fit_clean <- duane(times, failures)

  # Add ±1% random noise
  noise <- rnorm(length(times), mean = 0, sd = 1)
  times_noisy <- times + times * noise * 0.01
  fit_noisy <- duane(times_noisy, failures)

  slope_clean <- coef(fit_clean$model)[2]
  slope_noisy <- coef(fit_noisy$model)[2]

  expect_equal(slope_clean, slope_noisy, tolerance = 0.05)
})

test_that("adding small noise preserves fitted MTBF values", {
  times <- seq(100, 1000, length.out = 20)
  failures <- rep(1, 20)

  fit_clean <- duane(times, failures)

  noise <- rnorm(length(times), mean = 0, sd = 1)
  times_noisy <- times + times * noise * 0.01
  fit_noisy <- duane(times_noisy, failures)

  # Compare fitted values via correlation
  cor_val <- cor(fit_clean$Fitted_Values, fit_noisy$Fitted_Values)
  expect_gt(cor_val, 0.99)
})


