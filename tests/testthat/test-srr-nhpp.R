#' @srrstats {G5.0} Tests use standard data sets.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by stop() is unique.
#' @srrstats {G5.4} Correctness tests with known parameter values.
#' @srrstats {G5.6} Parameter recovery tests.
#' @srrstats {G5.8a} Checks for zero-length data.
#' @srrstats {G5.8b} Checks for unsupported data types.
#' @srrstats {G5.8c} Checks for data with NA fields.
#' @srrstats {G5.9a} Noise susceptibility tests.

# --- Input validation tests ---

test_that("nhpp errors on non-numeric time", {
  expect_error(
    nhpp(time = c("a", "b"), event = c(1, 1)),
    "'time' must be a numeric vector.", fixed = TRUE
  )
})

test_that("nhpp errors on empty time", {
  expect_error(nhpp(time = c()), "'time' must be a numeric vector.", fixed = TRUE)
})

test_that("nhpp errors on NA in time", {
  expect_error(
    nhpp(time = c(100, NA), event = c(1, 1)),
    "'time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("nhpp errors on NaN in time", {
  expect_error(
    nhpp(time = c(100, NaN), event = c(1, 1)),
    "'time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("nhpp errors on non-positive time", {
  expect_error(
    nhpp(time = c(0, 100)),
    "All values in 'time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("nhpp errors on infinite time", {
  expect_error(
    nhpp(time = c(100, Inf)),
    "All values in 'time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("nhpp errors on non-increasing time", {
  expect_error(
    nhpp(time = c(200, 100)),
    "'time' must be strictly increasing.", fixed = TRUE
  )
})

test_that("nhpp errors on non-numeric event", {
  expect_error(
    nhpp(time = c(100, 200), event = c("a", "b")),
    "'event' must be a numeric vector.", fixed = TRUE
  )
})

test_that("nhpp errors on mismatched event length", {
  expect_error(
    nhpp(time = c(100, 200), event = c(1)),
    "'event' and 'time' must have the same length.", fixed = TRUE
  )
})

test_that("nhpp errors on NA in event", {
  expect_error(
    nhpp(time = c(100, 200), event = c(1, NA)),
    "'event' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("nhpp errors on non-positive event", {
  expect_error(
    nhpp(time = c(100, 200), event = c(1, 0)),
    "All values in 'event' must be finite and > 0.", fixed = TRUE
  )
})

test_that("nhpp errors on invalid model_type", {
  expect_error(
    nhpp(time = c(100, 200, 300), event = c(1, 2, 1), model_type = 123),
    "'model_type' must be a single character string.", fixed = TRUE
  )
})

test_that("nhpp errors on LS with Log-Linear", {
  expect_error(
    nhpp(time = c(100, 200, 300), event = c(1, 2, 1),
         model_type = "Log-Linear", method = "LS"),
    "'method = \"LS\"' is not supported for model_type = \"Log-Linear\".", fixed = TRUE
  )
})

test_that("nhpp errors on MLE with piecewise Power Law", {
  expect_error(
    nhpp(time = c(100, 200, 300, 400, 500), event = c(1, 2, 1, 3, 2),
         breaks = c(250), method = "MLE"),
    "'method = \"MLE\"' is not supported for piecewise Power Law models.", fixed = TRUE
  )
})

test_that("nhpp errors on invalid breaks", {
  expect_error(
    nhpp(time = c(100, 200, 300, 400, 500), event = c(1, 2, 1, 3, 2),
         breaks = numeric(0), method = "LS"),
    "'breaks' must be a non-empty numeric vector if provided.", fixed = TRUE
  )
  expect_error(
    nhpp(time = c(100, 200, 300, 400, 500), event = c(1, 2, 1, 3, 2),
         breaks = c(-1), method = "LS"),
    "All values in 'breaks' must be finite and > 0.", fixed = TRUE
  )
})

test_that("nhpp errors on breaks with Log-Linear", {
  expect_error(
    nhpp(time = c(100, 200, 300, 400, 500), event = c(1, 2, 1, 3, 2),
         breaks = c(250), model_type = "Log-Linear"),
    "'breaks' can only be used with the 'Power Law' model.", fixed = TRUE
  )
})

test_that("nhpp errors on invalid conf_level", {
  expect_error(
    nhpp(time = c(100, 200, 300), event = c(1, 2, 1), conf_level = "a"),
    "'conf_level' must be a single numeric value.", fixed = TRUE
  )
  expect_error(
    nhpp(time = c(100, 200, 300), event = c(1, 2, 1), conf_level = 0),
    "'conf_level' must be between 0 and 1 (exclusive).", fixed = TRUE
  )
})

# --- Data frame input tests ---

test_that("nhpp works with data frame input", {
  df <- data.frame(time = c(100, 200, 300, 400, 500), event = c(1, 2, 1, 3, 2))
  result <- nhpp(data = df)
  expect_s3_class(result, "nhpp")
})

test_that("nhpp data frame errors on missing time column", {
  expect_error(
    nhpp(data = data.frame(x = 1:3)),
    "'data' must contain a column named 'time'.", fixed = TRUE
  )
})

test_that("nhpp data frame errors on non-data-frame", {
  expect_error(
    nhpp(data = list(time = 1:3)),
    "'data' must be a data frame.", fixed = TRUE
  )
})

# --- Power Law MLE correctness ---

test_that("Power Law MLE returns valid nhpp object", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)
  result <- nhpp(time, event, method = "MLE")

  expect_s3_class(result, "nhpp")
  expect_equal(result$model_type, "Power Law")
  expect_equal(result$method, "MLE")
  expect_true(!is.null(result$params$beta))
  expect_true(!is.null(result$params$lambda))
  expect_true(result$params$beta > 0)
  expect_true(result$params$lambda > 0)
  expect_equal(result$n_obs, 5)
  expect_equal(length(result$fitted_values), 5)
})

test_that("Power Law MLE parameter recovery", {
  # Generate data from known power law: N(t) = 0.02 * t^1.2
  set.seed(42)
  true_beta <- 1.2
  true_lambda <- 0.02
  time <- seq(100, 2000, by = 100)
  expected <- true_lambda * time^true_beta
  event <- pmax(1, round(diff(c(0, expected)) + rnorm(length(time), 0, 0.5)))

  result <- nhpp(time, event, method = "MLE")

  expect_equal(result$params$beta, true_beta, tolerance = 0.3)
  expect_equal(result$params$lambda, true_lambda, tolerance = 0.05)
})

test_that("Power Law MLE has valid GOF statistics", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)
  result <- nhpp(time, event, method = "MLE")

  expect_true(is.finite(result$logLik))
  expect_true(is.finite(result$AIC))
  expect_true(is.finite(result$BIC))
})

# --- Power Law LS correctness ---

test_that("Power Law LS returns valid nhpp object", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)
  result <- nhpp(time, event, method = "LS")

  expect_s3_class(result, "nhpp")
  expect_equal(result$model_type, "Power Law")
  expect_equal(result$method, "LS")
  expect_true(!is.null(result$params$beta))
  expect_true(!is.null(result$params$lambda))
  expect_true(!is.null(result$model))
})

test_that("Power Law LS and MLE produce similar estimates", {
  time <- c(200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000)
  event <- c(3, 5, 4, 7, 6, 8, 5, 9, 7, 10)

  result_ls <- nhpp(time, event, method = "LS")
  result_mle <- nhpp(time, event, method = "MLE")

  expect_equal(unname(result_ls$params$beta), result_mle$params$beta, tolerance = 0.5)
})

# --- Power Law LS with default event ---

test_that("nhpp uses default event of all 1s when event is NULL", {
  time <- c(100, 200, 300, 400, 500)
  result <- nhpp(time)
  expect_s3_class(result, "nhpp")
  expect_equal(result$event, rep(1, 5))
  expect_equal(result$cum_events, 1:5)
})

# --- Log-Linear MLE correctness ---

test_that("Log-Linear MLE returns valid nhpp object", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)
  result <- nhpp(time, event, model_type = "Log-Linear")

  expect_s3_class(result, "nhpp")
  expect_equal(result$model_type, "Log-Linear")
  expect_equal(result$method, "MLE")
  expect_true(!is.null(result$params$a))
  expect_true(!is.null(result$params$b))
  expect_true(is.finite(result$logLik))
})

test_that("Log-Linear MLE fitted values are positive", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)
  result <- nhpp(time, event, model_type = "Log-Linear")
  expect_true(all(result$fitted_values > 0))
})

# --- Segmented Power Law LS ---

test_that("Piecewise Power Law with breaks works", {
  time <- c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000,
            1100, 1200, 1300, 1400, 1500)
  event <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)
  result <- nhpp(time, event, breaks = c(500), method = "LS")

  expect_s3_class(result, "nhpp")
  expect_true(!is.null(result$breakpoints))
  expect_true(length(result$params$betas) > 1)
})

# --- Confidence bounds ---

test_that("nhpp confidence bounds widen with higher conf_level", {
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)

  result_90 <- nhpp(time, event, conf_level = 0.90)
  result_99 <- nhpp(time, event, conf_level = 0.99)

  width_90 <- result_90$upper_bounds - result_90$lower_bounds
  width_99 <- result_99$upper_bounds - result_99$lower_bounds

  expect_true(all(width_99 >= width_90))
})

# --- Noise susceptibility ---

test_that("Power Law MLE is stable with small noise", {
  skip_on_cran()
  set.seed(123)
  time <- c(200, 400, 600, 800, 1000)
  event <- c(3, 5, 4, 7, 6)

  result_clean <- nhpp(time, event, method = "MLE")

  # Add small noise to event counts (round to keep integers)
  event_noisy <- pmax(1, round(event + rnorm(5, 0, 0.3)))
  result_noisy <- nhpp(time, event_noisy, method = "MLE")

  expect_equal(result_clean$params$beta, result_noisy$params$beta, tolerance = 0.5)
})

# --- predict_nhpp tests ---

test_that("predict_nhpp errors on wrong class", {
  expect_error(
    predict_nhpp(list(a = 1), time = c(1500)),
    "'object' must be an object of class 'nhpp'.", fixed = TRUE
  )
})

test_that("predict_nhpp errors on non-numeric time", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(
    predict_nhpp(fit, time = c("a")),
    "'time' must be a numeric vector.", fixed = TRUE
  )
})

test_that("predict_nhpp errors on empty time", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(
    predict_nhpp(fit, time = c()),
    "'time' must be a numeric vector.", fixed = TRUE
  )
})

test_that("predict_nhpp errors on NA in time", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(
    predict_nhpp(fit, time = c(1500, NA)),
    "'time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("predict_nhpp errors on non-positive time", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(
    predict_nhpp(fit, time = c(0, 1500)),
    "All values in forecast 'time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("predict_nhpp errors on invalid conf_level", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(
    predict_nhpp(fit, time = c(1500), conf_level = 1.5),
    "'conf_level' must be between 0 and 1 (exclusive).", fixed = TRUE
  )
})

test_that("predict_nhpp warns on hindcasting", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_warning(
    predict_nhpp(fit, time = c(500)),
    "Some 'time' values are <= the maximum observed time"
  )
})

test_that("predict_nhpp returns valid nhpp_predict for Power Law MLE", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6), method = "MLE")
  fc <- predict_nhpp(fit, time = c(1500, 2000))

  expect_s3_class(fc, "nhpp_predict")
  expect_equal(length(fc$cum_events), 2)
  expect_true(all(fc$cum_events > 0))
  expect_true(all(fc$lower_bounds < fc$cum_events))
  expect_true(all(fc$upper_bounds > fc$cum_events))
})

test_that("predict_nhpp returns valid nhpp_predict for Power Law LS", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6), method = "LS")
  fc <- predict_nhpp(fit, time = c(1500, 2000))

  expect_s3_class(fc, "nhpp_predict")
  expect_equal(length(fc$cum_events), 2)
  expect_true(all(fc$cum_events > 0))
})

test_that("predict_nhpp returns valid nhpp_predict for Log-Linear", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6),
              model_type = "Log-Linear")
  fc <- predict_nhpp(fit, time = c(1500, 2000))

  expect_s3_class(fc, "nhpp_predict")
  expect_equal(length(fc$cum_events), 2)
  expect_true(all(fc$cum_events > 0))
})

test_that("predict_nhpp forecast is monotonically increasing for Power Law", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6), method = "MLE")
  fc <- predict_nhpp(fit, time = c(1200, 1500, 2000, 3000))
  expect_true(all(diff(fc$cum_events) > 0))
})

# --- Print and plot tests ---

test_that("print.nhpp produces output for Power Law MLE", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6), method = "MLE")
  expect_output(print(result), "Non-Homogeneous Poisson Process")
  expect_output(print(result), "Power Law")
  expect_output(print(result), "Beta:")
})

test_that("print.nhpp produces output for Log-Linear", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6),
                 model_type = "Log-Linear")
  expect_output(print(result), "Log-Linear")
})

test_that("print.nhpp returns invisible", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_invisible(print(result))
})

test_that("print.nhpp errors on wrong class", {
  expect_error(
    print.nhpp(list(a = 1)),
    "'x' must be an object of class 'nhpp'.", fixed = TRUE
  )
})

test_that("plot.nhpp produces a plot without error", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_silent(plot(result, main = "Test NHPP"))
})

test_that("plot.nhpp validates inputs", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(plot.nhpp(list(a = 1)), "'x' must be an object of class 'nhpp'.")
  expect_error(plot(result, conf_bounds = "yes"), "'conf_bounds' must be a single logical value.")
  expect_error(plot(result, legend = "yes"), "'legend' must be a single logical value.")
  expect_error(plot(result, legend_pos = 123), "'legend_pos' must be a single character string.")
})

test_that("plot.nhpp works without conf bounds and legend", {
  result <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_silent(plot(result, conf_bounds = FALSE, legend = FALSE))
})

test_that("print.nhpp_predict produces output", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  fc <- predict_nhpp(fit, time = c(1500, 2000))
  expect_output(print(fc), "NHPP Forecast")
})

test_that("print.nhpp_predict returns invisible", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  fc <- predict_nhpp(fit, time = c(1500, 2000))
  expect_invisible(print(fc))
})

test_that("print.nhpp_predict errors on wrong class", {
  expect_error(
    print.nhpp_predict(list(a = 1)),
    "'x' must be an object of class 'nhpp_predict'.", fixed = TRUE
  )
})

test_that("plot.nhpp_predict produces a plot without error", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  fc <- predict_nhpp(fit, time = c(1500, 2000))
  expect_silent(plot(fc, main = "Test Forecast"))
})

test_that("plot.nhpp_predict validates inputs", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  fc <- predict_nhpp(fit, time = c(1500, 2000))

  expect_error(plot.nhpp_predict(list(a = 1)), "'x' must be an object of class 'nhpp_predict'.")
  expect_error(plot(fc, conf_bounds = "yes"), "'conf_bounds' must be a single logical value.")
  expect_error(plot(fc, legend = "yes"), "'legend' must be a single logical value.")
  expect_error(plot(fc, legend_pos = 123), "'legend_pos' must be a single character string.")
})

test_that("plot.nhpp_predict works without conf bounds and legend", {
  fit <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  fc <- predict_nhpp(fit, time = c(1500, 2000))
  expect_silent(plot(fc, conf_bounds = FALSE, legend = FALSE))
})

# ---- overlay_nhpp tests ----
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by stop() is unique.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {RE6.0} Model objects have overlay plot methods.
#' @srrstats {RE6.2} The overlay plot shows fitted values with optional CIs.

test_that("overlay_nhpp: non-list input errors", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(overlay_nhpp(m), "'models' must be a non-empty list of 'nhpp' objects.", fixed = TRUE)
})

test_that("overlay_nhpp: empty list errors", {
  expect_error(overlay_nhpp(list()), "'models' must be a non-empty list of 'nhpp' objects.", fixed = TRUE)
})

test_that("overlay_nhpp: non-nhpp element errors", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(overlay_nhpp(list(m, list(a = 1))),
               "All elements of 'models' must be objects of class 'nhpp'.", fixed = TRUE)
})

test_that("overlay_nhpp: conf_bounds wrong type errors", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(overlay_nhpp(list(m), conf_bounds = "yes"),
               "'conf_bounds' must be a single logical value.", fixed = TRUE)
})

test_that("overlay_nhpp: legend wrong type errors", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(overlay_nhpp(list(m), legend = 1L),
               "'legend' must be a single logical value.", fixed = TRUE)
})

test_that("overlay_nhpp: legend_pos wrong length errors", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  expect_error(overlay_nhpp(list(m), legend_pos = c("a", "b")),
               "'legend_pos' must be a single character string.", fixed = TRUE)
})

test_that("overlay_nhpp: colors too short errors", {
  m1 <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  expect_error(overlay_nhpp(list(m1, m2), colors = "black"),
               "'colors' must be a character vector with at least one color per model.", fixed = TRUE)
})

test_that("overlay_nhpp: single model renders without error", {
  m <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_nhpp(list(m)))
})

test_that("overlay_nhpp: two named models render and return NULL", {
  m1 <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  pdf(NULL)
  on.exit(dev.off())
  result <- overlay_nhpp(list(System_A = m1, System_B = m2))
  expect_null(result)
})

test_that("overlay_nhpp: two unnamed models render without error", {
  m1 <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_nhpp(list(m1, m2)))
})

test_that("overlay_nhpp: conf_bounds = FALSE renders without error", {
  m1 <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_nhpp(list(m1, m2), conf_bounds = FALSE))
})

test_that("overlay_nhpp: custom colors render without error", {
  m1 <- nhpp(c(200, 400, 600, 800, 1000), c(3, 5, 4, 7, 6))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_nhpp(list(m1, m2), colors = c("steelblue", "tomato")))
})

test_that("overlay_nhpp: models with different-length datasets render without error", {
  m1 <- nhpp(c(200, 400, 600), c(3, 5, 4))
  m2 <- nhpp(c(300, 600, 900, 1200, 1500), c(4, 6, 5, 8, 7))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_nhpp(list(m1, m2)))
})
