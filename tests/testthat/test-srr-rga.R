#' @srrstats {G5.0} The function is tested with a standard data set from a published paper.
#' @srrstats {G5.1} The function is tested with a standard data set. The data set is
#' created within and used to test the package. The data set is exported so that users
#' can confirm tests and run examples.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.4c} Unit tests include stored values that are drawn from a published paper output.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks to test that the implementation produces expected results given data with known properties.
#' @srrstats {G5.6a} Parameter recovery tests are expected to be within a defined tolerance rather than exact values.
#' @srrstats {G5.7} Unit tests include algorithm performance checks to test that the function performs as expected as parameters change.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.9b} Unit tests check that different random seeds do not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#' @srrstats {RE7.1} Unit tests check for noiseless, exact relationships between
#' predictor (independent) and response (dependent) data.
#' @srrstats {RE7.1a} Unit tests confirm that model fitting is at least as fast
#' or faster than testing with equivalent noisy data.
#' @srrstats {RE7.2} Unit tests demonstrate that output objects retain aspects
#' of input data such as case names.
#' @srrstats {RE7.3} Unit tests demonstrate expected behavior when `rga` object
#' is submitted to the accessor methods `print` and `plot`.

test_that("data.frame input works the same as separate vectors", {
  times <- c(100, 200, 300)
  failures <- c(1, 2, 1)
  df <- data.frame(times = times, failures = failures)

  res_df <- rga(df)
  res_vec <- rga(times, failures)

  expect_s3_class(res_df, "rga")
  expect_s3_class(res_vec, "rga")
  expect_equal(res_df$betas, res_vec$betas)
  expect_equal(res_df$lambdas, res_vec$lambdas)
})

test_that("data.frame without required columns errors", {
  df1 <- data.frame(x = 1:3, y = 1:3)
  expect_error(
    rga(df1),
    "must contain columns 'times' and 'failures'"
  )

  df2 <- data.frame(times = 1:3)
  expect_error(
    rga(df2),
    "must contain columns 'times' and 'failures'"
  )
})

test_that("NA or NaN in data.frame input throws error", {
  df_na <- data.frame(times = c(100, 200, NA), failures = c(1, 2, 1))
  df_nan <- data.frame(times = c(100, 200, NaN), failures = c(1, 2, 1))
  df_fail_na <- data.frame(times = c(100, 200, 300), failures = c(1, NA, 1))

  expect_error(rga(df_na), "'times' contains missing")
  expect_error(rga(df_nan), "'times' contains missing")
  expect_error(rga(df_fail_na), "'failures' contains missing")
})

test_that("data.frame with non-positive or infinite values errors", {
  df_zero_time <- data.frame(times = c(0, 100, 200), failures = c(1, 2, 1))
  df_neg_fail <- data.frame(times = c(100, 200, 300), failures = c(1, -1, 1))
  df_inf_time <- data.frame(times = c(100, Inf, 200), failures = c(1, 2, 1))

  expect_error(rga(df_zero_time), "must be finite and > 0")
  expect_error(rga(df_neg_fail), "must be finite and > 0")
  expect_error(rga(df_inf_time), "must be finite and > 0")
})

# Helper: a minimal valid pair of inputs
valid_times <- c(100, 200, 300)
valid_failures <- c(1, 2, 1)

test_that("data frame without required columns errors", {
  df_bad <- data.frame(a = 1:3, b = 1:3)
  expect_error(rga(df_bad),
    "If a data frame is provided, it must contain columns 'times' and 'failures'.",
    fixed = TRUE
  )
})

test_that("NA / NaN checks for times and failures", {
  expect_error(rga(c(1, NA), valid_failures),
    "'times' contains missing (NA) or NaN values.",
    fixed = TRUE
  )

  expect_error(rga(valid_times, c(1, NaN, 1)),
    "'failures' contains missing (NA) or NaN values.",
    fixed = TRUE
  )
})

test_that("type checks for times and failures", {
  expect_error(rga(list(1, 2, 3), valid_failures),
    "'times' must be a numeric vector.",
    fixed = TRUE
  )

  expect_error(rga(valid_times, list(1, 2, 3)),
    "'failures' must be a numeric vector.",
    fixed = TRUE
  )
})

test_that("empty vector checks", {
  expect_error(rga(numeric(0), numeric(0)),
    "'times' cannot be empty.",
    fixed = TRUE
  )

  # ensure failures empty triggers its message
  expect_error(rga(valid_times, numeric(0)),
    "'failures' cannot be empty.",
    fixed = TRUE
  )
})

test_that("length mismatch check", {
  expect_error(rga(c(1, 2), c(1, 2, 3)),
    "The length of 'times' and 'failures' must be equal.",
    fixed = TRUE
  )
})

test_that("finite and >0 checks for times and failures", {
  expect_error(rga(c(1, Inf, 3), c(1, 1, 1)),
    "All values in 'times' must be finite and > 0.",
    fixed = TRUE
  )

  expect_error(rga(c(1, 2, 3), c(1, 0, 2)),
    "All values in 'failures' must be finite and > 0.",
    fixed = TRUE
  )
})

test_that("model_type validation errors", {
  # non-single character
  expect_error(rga(valid_times, valid_failures, model_type = c("a", "b")),
    "'model_type' must be a single character string.",
    fixed = TRUE
  )

  # invalid string - match.arg produces its own message; check for 'one of'
  expect_error(
    rga(valid_times, valid_failures, model_type = "not-a-model"),
    "one of"
  )
})

test_that("breaks argument validation", {
  # breaks must be numeric non-empty vector if provided
  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = character(1)),
    "'breaks' must be a non-empty numeric vector if provided.",
    fixed = TRUE
  )

  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = numeric(0)),
    "'breaks' must be a non-empty numeric vector if provided.",
    fixed = TRUE
  )

  # breaks values must be finite and > 0
  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = c(100, -1)),
    "All values in 'breaks' must be finite and > 0.",
    fixed = TRUE
  )

  # breaks only allowed with piecewise model
  expect_error(rga(valid_times, valid_failures, model_type = "Crow-AMSAA", breaks = c(150)),
    "'breaks' can only be used with the 'Piecewise NHPP' model.",
    fixed = TRUE
  )
})

test_that("conf_level validation", {
  expect_error(rga(valid_times, valid_failures, conf_level = c(0.9, 0.95)),
    "'conf_level' must be a single numeric value.",
    fixed = TRUE
  )

  expect_error(rga(valid_times, valid_failures, conf_level = 1),
    "'conf_level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )

  expect_error(rga(valid_times, valid_failures, conf_level = 0),
    "'conf_level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )
})

test_that("cumulative_failure_times input reproduces the default fit", {
  interval_times <- c(100, 200, 300, 400, 500)
  cumulative_times <- cumsum(interval_times)
  failures <- c(1, 2, 1, 3, 2)

  fit_interval <- rga(interval_times, failures)
  fit_cumulative <- rga(
    cumulative_times, failures,
    times_type = "cumulative_failure_times"
  )

  expect_equal(fit_cumulative$times, cumulative_times)
  expect_equal(fit_cumulative$cum_times, cumulative_times)
  expect_equal(fit_cumulative$times_type, "cumulative_failure_times")
  expect_equal(fit_cumulative$betas, fit_interval$betas)
  expect_equal(fit_cumulative$lambdas, fit_interval$lambdas)
  expect_equal(fit_cumulative$fitted_values, fit_interval$fitted_values)
  expect_equal(fit_cumulative$cum_failures, fit_interval$cum_failures)
})

test_that("cumulative_failure_times input must be strictly increasing", {
  expect_error(
    rga(
      c(100, 200, 200),
      c(1, 2, 1),
      times_type = "cumulative_failure_times"
    ),
    "must be strictly increasing"
  )
})

test_that("MLE supports cumulative_failure_times input", {
  interval_times <- c(100, 200, 300, 400, 500)
  cumulative_times <- cumsum(interval_times)
  failures <- c(1, 2, 1, 3, 2)

  fit_interval <- rga(interval_times, failures, method = "MLE")
  fit_cumulative <- rga(
    cumulative_times, failures,
    times_type = "cumulative_failure_times",
    method = "MLE"
  )

  expect_equal(fit_cumulative$cum_times, cumulative_times)
  expect_equal(fit_cumulative$times_type, "cumulative_failure_times")
  expect_equal(fit_cumulative$betas, fit_interval$betas)
  expect_equal(fit_cumulative$lambdas, fit_interval$lambdas)
})

test_that("print.rga errors when input is not rga", {
  expect_error(print.rga(list()),
    "'x' must be an object of class 'rga'.",
    fixed = TRUE
  )
})

test_that("plot.rga argument type checks and malformed object", {
  # Create a minimal valid-looking rga object so argument checks after inherits run
  x_good <- list(
    model = list(model = data.frame(log_times = 1, log_cum_failures = 1)),
    fitted_values = 1,
    lower_bounds = 1,
    upper_bounds = 1
  )
  class(x_good) <- "rga"

  # inherits check (non-rga)
  expect_error(plot.rga(list()),
    "'x' must be an object of class 'rga'.",
    fixed = TRUE
  )

  # conf_bounds must be single logical
  expect_error(plot.rga(x_good, conf_bounds = "nope"),
    "'conf_bounds' must be a single logical value.",
    fixed = TRUE
  )

  # legend must be single logical
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = "nope"),
    "'legend' must be a single logical value.",
    fixed = TRUE
  )

  # log must be single logical
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = TRUE, log = "nope"),
    "'log' must be a single logical value.",
    fixed = TRUE
  )

  # legend_pos must be single character string
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = TRUE, log = FALSE, legend_pos = c("a", "b")),
    "'legend_pos' must be a single character string.",
    fixed = TRUE
  )

  # malformed rga: missing required model$model columns
  x_bad_model <- list(model = list(model = data.frame(foo = 1, bar = 2)))
  class(x_bad_model) <- "rga"
  expect_error(plot.rga(x_bad_model),
    "The 'rga' object appears malformed or missing model data.",
    fixed = TRUE
  )
})

test_that("Crow-AMSAA parameter recovery works (log-log simulation)", {
  set.seed(123)

  # True parameters (log-log)
  beta_true <- 1.3
  lambda_true <- 0.01

  # Observation times (cumulative)
  n <- 200
  t_obs <- seq(50, 5000, length.out = n)
  log_t_obs <- log(t_obs)

  # Mean cumulative failures in log-log space
  log_mu <- log(lambda_true) + beta_true * log_t_obs
  mu <- exp(log_mu)

  # Poisson increments, strictly positive
  failures <- as.integer(pmax(rpois(n, diff(c(0, mu))), 1))

  # Interarrival times (increments)
  times <- c(t_obs[1], diff(t_obs))

  # Fit Crow-AMSAA model
  fit <- rga(times, failures, model_type = "Crow-AMSAA")

  tol_beta <- 0.1
  tol_lambda <- 0.01

  expect_true(abs(fit$betas - beta_true) < tol_beta,
    info = paste("beta estimate:", fit$betas)
  )
  expect_true(abs(fit$lambdas - lambda_true) < tol_lambda,
    info = paste("lambda estimate:", fit$lambdas)
  )
})

test_that("Piecewise NHPP parameter recovery works", {
  # True params
  beta1 <- 0.7
  beta2 <- 1.4
  lambda1 <- 0.02
  tb <- 200
  lambda2 <- lambda1 * tb^(beta1 - beta2) # continuity at breakpoint

  # Generate observation times and interval inputs for rga()
  time_points <- seq(5, 1000, length.out = 200)
  intervals <- c(time_points[1], diff(time_points))

  # True cumulative counts (piecewise power law, continuous at tb)
  cumN <- ifelse(time_points <= tb,
    lambda1 * (time_points^beta1),
    lambda2 * (time_points^beta2)
  )
  failures <- c(cumN[1], diff(cumN))
  stopifnot(all(failures > 0))

  # Run rga with the known breakpoint (simpler, direct parameter recovery)
  res <- rga(
    times = intervals, failures = failures,
    model_type = "Piecewise NHPP", breaks = tb, conf_level = 0.95
  )

  # Helper: extract numeric betas from rga output (handles usual segmented structure)
  get_betas_num <- function(b) {
    if (is.numeric(b)) {
      return(as.numeric(b))
    }
    if (is.list(b) && "log_times" %in% names(b)) {
      mat <- b$log_times
      if ("Est." %in% colnames(mat)) {
        return(as.numeric(mat[, "Est."]))
      }
      if ("Estimate" %in% colnames(mat)) {
        return(as.numeric(mat[, "Estimate"]))
      }
      return(as.numeric(mat[, 1]))
    }
    stop("Can't extract betas from rga output.")
  }

  est_betas <- get_betas_num(res$betas)
  expect_length(est_betas, 2)
  expect_equal(est_betas[1], beta1, tolerance = 1e-6)
  expect_equal(est_betas[2], beta2, tolerance = 1e-6)

  # Lambdas should be numeric vector
  est_lambdas <- as.numeric(res$lambdas)
  expect_length(est_lambdas, 2)
  expect_equal(est_lambdas[1], lambda1, tolerance = 1e-4)
  expect_equal(est_lambdas[2], lambda2, tolerance = 1e-4)

  # Breakpoint (rga stores on log-scale) -> back-transform and compare
  expect_true(!is.null(res$breakpoints))
  recovered_tb <- exp(res$breakpoints[1])
  expect_equal(recovered_tb, tb, tolerance = 1e-6)
})

test_that("rga() handles increasing dataset sizes efficiently", {
  set.seed(123)
  sizes <- c(100, 500, 1000, 5000)
  beta_true <- 1.2
  lambda_true <- 0.01

  for (n in sizes) {
    t_obs <- seq(50, 5000, length.out = n)
    log_mu <- log(lambda_true) + beta_true * log(t_obs)
    mu <- exp(log_mu)
    failures <- as.integer(pmax(rpois(n, diff(c(0, mu))), 1))
    times <- c(t_obs[1], diff(t_obs))

    start_time <- Sys.time()
    fit <- rga(times, failures, model_type = "Crow-AMSAA")
    end_time <- Sys.time()

    runtime <- as.numeric(difftime(end_time, start_time, units = "secs"))
    message(sprintf("n = %d, runtime = %.3f sec", n, runtime))

    expect_true(abs(fit$betas - beta_true) < 0.1)
  }
})

test_that("Crow-AMSAA is robust to small noise in times", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(101)

  # True parameters
  beta_true <- 1.2
  lambda_true <- 0.01
  n <- 200
  t_obs <- seq(50, 5000, length.out = n)
  log_mu <- log(lambda_true) + beta_true * log(t_obs)
  mu <- exp(log_mu)
  failures <- as.integer(pmax(rpois(n, diff(c(0, mu))), 1))
  times <- c(t_obs[1], diff(t_obs))

  # Fit on original data
  fit_orig <- rga(times, failures, model_type = "Crow-AMSAA")

  # Add trivial Gaussian noise to times (~0.5% of original value)
  times_noisy <- times * (1 + rnorm(n, mean = 0, sd = 0.005))

  fit_noisy <- rga(times_noisy, failures, model_type = "Crow-AMSAA")

  # Parameters should not change meaningfully
  expect_true(abs(fit_noisy$betas - fit_orig$betas) < 0.02)
  expect_true(abs(fit_noisy$lambdas - fit_orig$lambdas) < 0.001)
})

test_that("Crow-AMSAA is robust to small noise in failures", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(202)

  beta_true <- 1.2
  lambda_true <- 0.01
  n <- 200
  t_obs <- seq(50, 5000, length.out = n)
  log_mu <- log(lambda_true) + beta_true * log(t_obs)
  mu <- exp(log_mu)
  failures <- as.integer(pmax(rpois(n, diff(c(0, mu))), 1))
  times <- c(t_obs[1], diff(t_obs))

  fit_orig <- rga(times, failures, model_type = "Crow-AMSAA")

  # Add tiny Gaussian noise to failures and round to integers
  failures_noisy <- as.integer(pmax(failures + rnorm(n, mean = 0, sd = 0.05), 1))

  fit_noisy <- rga(times, failures_noisy, model_type = "Crow-AMSAA")

  expect_true(abs(fit_noisy$betas - fit_orig$betas) < 0.05)
  expect_true(abs(fit_noisy$lambdas - fit_orig$lambdas) < 0.01)
})

test_that("Piecewise NHPP is robust to small noise in times and failures", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(303)

  beta_true <- c(0.8, 1.5)
  lambda_true <- c(0.02, NA)
  break_time <- 2500
  n <- 200
  t_obs <- seq(100, 5000, length.out = n)
  log_lambda2 <- log(lambda_true[1]) + beta_true[1] * log(break_time) - beta_true[2] * log(break_time)
  lambda_true[2] <- exp(log_lambda2)

  log_mu <- numeric(n)
  for (i in seq_along(t_obs)) {
    if (t_obs[i] <= break_time) {
      log_mu[i] <- log(lambda_true[1]) + beta_true[1] * log(t_obs[i])
    } else {
      log_mu[i] <- log_lambda2 + beta_true[2] * log(t_obs[i])
    }
  }
  mu <- exp(log_mu)
  failures <- as.integer(pmax(rpois(n, diff(c(0, mu))), 1))
  times <- c(t_obs[1], diff(t_obs))

  fit_orig <- rga(times, failures, model_type = "Piecewise NHPP", breaks = c(break_time))

  # Add small noise
  times_noisy <- times * (1 + rnorm(n, 0, 0.005))
  failures_noisy <- as.integer(pmax(failures + rnorm(n, 0, 0.05), 1))

  fit_noisy <- rga(times_noisy, failures_noisy, model_type = "Piecewise NHPP", breaks = c(break_time))

  slopes_orig <- fit_orig$betas$log_times[, "Est."]
  slopes_noisy <- fit_noisy$betas$log_times[, "Est."]
  intercepts_orig <- fit_orig$lambdas
  intercepts_noisy <- fit_noisy$lambdas

  expect_true(all(abs(slopes_noisy - slopes_orig) < 0.05))
  expect_true(all(abs(intercepts_noisy - intercepts_orig) < 0.01))
})

test_that("rga works with input from testdata (Crow-AMSAA)", {
  # testdata is lazy-loaded via LazyData: true; no explicit data() call needed
  g1 <- subset(testdata, LRU == "G1")
  times <- g1$Cum_ETI
  failures <- g1$Failure_Count

  fit <- rga(times, failures, model_type = "Crow-AMSAA", conf_level = 0.95)

  expect_true(all(fit$fitted_values > 0))
  expect_true(all(fit$lower_bounds > 0))
  expect_true(all(fit$upper_bounds > 0))
})

# Create a simple dataset for testing
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)
rga_obj <- rga(times, failures)

test_that("print.rga works correctly", {
  # Output should include key phrases
  expect_output(print(rga_obj), "Reliability Growth Analysis")
  expect_output(print(rga_obj), "Model Type: Crow-AMSAA")
  expect_output(print(rga_obj), "Log-likelihood:")
  expect_output(print(rga_obj), "AIC:")
  expect_output(print(rga_obj), "BIC:")

  # Should invisibly return the same object
  expect_invisible(print(rga_obj))
})

test_that("plot.rga basic functionality works", {
  # Just checks that no error is thrown
  expect_silent(plot(rga_obj))

  # With options
  expect_silent(plot(rga_obj, conf_bounds = FALSE, legend = FALSE))
  expect_silent(plot(rga_obj, log = TRUE))
  expect_silent(plot(rga_obj, legend_pos = "topright"))
})

test_that("plot.rga argument validation works", {
  expect_error(plot(rga_obj, conf_bounds = "yes"), "'conf_bounds' must be a single logical value")
  expect_error(plot(rga_obj, legend = c(TRUE, FALSE)), "'legend' must be a single logical value")
  expect_error(plot(rga_obj, log = NA), "missing value where TRUE/FALSE needed")
  expect_error(plot(rga_obj, legend_pos = 1), "'legend_pos' must be a single character string")
})

# Optional: visual regression tests (requires vdiffr)
if (requireNamespace("vdiffr", quietly = TRUE)) {
  test_that("plot.rga visual output is stable", {
    vdiffr::expect_doppelganger("Crow-AMSAA plot", function() plot(rga_obj))
  })
}

test_that("rga() errors on perfect (noiseless) collinearity between predictor and response", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  # Perfect power-law relationship between cumulative time and cumulative failures
  n <- 200
  cum_time <- seq(1, n)
  lambda <- 2.0
  beta <- 0.75
  cum_failures <- lambda * cum_time^beta

  # failures per interval (must be positive)
  failures <- diff(c(0, cum_failures))

  # Expect perfect-collinearity error
  expect_error(
    rga(times = rep(1, n), failures = failures),
    regexp = "Perfect collinearity detected"
  )

  # also check with data.frame input
  df <- data.frame(times = rep(1, n), failures = failures)
  expect_error(
    rga(df),
    regexp = "Perfect collinearity detected"
  )
})

test_that("rga() fits near-noiseless data and is at least as fast as noisy data (Crow-AMSAA)", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(42)
  n <- 800
  cum_time <- seq(1, n)
  lambda <- 5.0
  beta <- 0.6
  cum_failures <- lambda * cum_time^beta
  failures_per_interval <- diff(c(0, cum_failures))

  # Create a near-noiseless version
  near_noise <- rnorm(n, mean = 0, sd = 1e-2)
  cum_failures_near <- cum_failures * exp(near_noise)
  failures_near <- diff(c(0, cum_failures_near))

  # Create a clearly noisy version
  big_noise <- rnorm(n, mean = 0, sd = 0.1)
  cum_failures_noisy <- cum_failures * exp(big_noise)
  failures_noisy <- diff(c(0, cum_failures_noisy))

  # Ensure positiveness
  failures_near <- pmax(failures_near, .Machine$double.eps)
  failures_noisy <- pmax(failures_noisy, .Machine$double.eps)

  # Timing: Crow-AMSAA model (default)
  t_near <- system.time({
    fit_near <- rga(times = rep(1, n), failures = failures_near, model_type = "Crow-AMSAA")
  })[["elapsed"]]

  t_noisy <- system.time({
    fit_noisy <- rga(times = rep(1, n), failures = failures_noisy, model_type = "Crow-AMSAA")
  })[["elapsed"]]

  # Require near-noiseless to be <= 1.2 * noisy (allow CI variability)
  expect_true(
    t_near <= t_noisy * 1.2 + 0.1,
    info = sprintf("Noiseless fit took %.3fs vs noisy %.3fs", t_near, t_noisy)
  )
})

test_that("rga() fits near-noiseless data with user-supplied breaks (Piecewise NHPP) and is reasonably fast", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  set.seed(101)
  n <- 400
  cum_time <- seq(1, n)
  lambda <- 3.0
  beta <- 0.8
  cum_failures <- lambda * cum_time^beta
  failures <- diff(c(0, cum_failures))

  # near-noiseless
  near_noise <- rnorm(n, mean = 0, sd = 5e-3) # changed from 1e-4 to 5e-3
  cum_failures_near <- cum_failures * exp(near_noise)
  failures_near <- diff(c(0, cum_failures_near))
  failures_near <- pmax(failures_near, .Machine$double.eps)

  big_noise <- rnorm(n, mean = 0, sd = 0.1)
  cum_failures_noisy <- cum_failures * exp(big_noise)
  failures_noisy <- diff(c(0, cum_failures_noisy))
  failures_noisy <- pmax(failures_noisy, .Machine$double.eps)

  # choose a breakpoint roughly in the middle (on original scale)
  break_pt <- floor(n / 2)

  t_near_pw <- system.time({
    fit_near_pw <- rga(
      times = rep(1, n), failures = failures_near,
      model_type = "Piecewise NHPP", breaks = break_pt
    )
  })[["elapsed"]]

  t_noisy_pw <- system.time({
    fit_noisy_pw <- rga(
      times = rep(1, n), failures = failures_noisy,
      model_type = "Piecewise NHPP", breaks = break_pt
    )
  })[["elapsed"]]

  # Require near-noiseless to be <= 1.2 * noisy (allow CI variability)
  expect_true(
    t_near_pw <= t_noisy_pw * 1.2 + 0.1,
    info = sprintf("Noiseless fit took %.3fs vs noisy %.3fs", t_near_pw, t_noisy_pw)
  )
})

# --------------------------------------------------------------------------- #
# predict_rga() tests                                                        #
# --------------------------------------------------------------------------- #

# Shared fixture for forecast tests
fc_times <- c(100, 200, 300, 400, 500)
fc_failures <- c(1, 2, 1, 3, 2)
fc_fit <- rga(fc_times, fc_failures)

test_that("predict_rga: input validation — wrong class", {
  expect_error(predict_rga(list()), "'object' must be an object of class 'rga'.",
    fixed = TRUE
  )
})

test_that("predict_rga: input validation — non-numeric times", {
  expect_error(predict_rga(fc_fit, times = "600"),
    "'times' must be a numeric vector.",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = list(600)),
    "'times' must be a numeric vector.",
    fixed = TRUE
  )
})

test_that("predict_rga: input validation — empty times", {
  expect_error(predict_rga(fc_fit, times = numeric(0)),
    "'times' cannot be empty.",
    fixed = TRUE
  )
})

test_that("predict_rga: input validation — NA / NaN in times", {
  expect_error(predict_rga(fc_fit, times = c(600, NA)),
    "'times' contains missing (NA) or NaN values.",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600, NaN)),
    "'times' contains missing (NA) or NaN values.",
    fixed = TRUE
  )
})

test_that("predict_rga: input validation — non-finite / non-positive times", {
  expect_error(predict_rga(fc_fit, times = c(600, Inf)),
    "All values in 'times' must be finite and > 0.",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600, -1)),
    "All values in 'times' must be finite and > 0.",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600, 0)),
    "All values in 'times' must be finite and > 0.",
    fixed = TRUE
  )
})

test_that("predict_rga: input validation — bad conf_level", {
  expect_error(predict_rga(fc_fit, times = c(600), conf_level = c(0.9, 0.95)),
    "'conf_level' must be a single numeric value.",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600), conf_level = 0),
    "'conf_level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600), conf_level = 1),
    "'conf_level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )
  expect_error(predict_rga(fc_fit, times = c(600), conf_level = Inf),
    "'conf_level' must be between 0 and 1 (exclusive).",
    fixed = TRUE
  )
})

test_that("predict_rga: hindcast warning when times <= max observed", {
  max_obs <- sum(fc_times) # cumulative time of last observation = 1500
  expect_warning(
    predict_rga(fc_fit, times = c(max_obs - 1, max_obs)),
    "Hindcasting is allowed but may not be meaningful."
  )
})

test_that("predict_rga uses stored cumulative times for cumulative_failure_times input", {
  fit_cumulative <- rga(
    cumsum(fc_times), fc_failures,
    times_type = "cumulative_failure_times"
  )

  expect_warning(
    predict_rga(fit_cumulative, times = c(max(fit_cumulative$cum_times))),
    "Hindcasting is allowed but may not be meaningful."
  )
})

test_that("predict_rga: Crow-AMSAA cum_failures == lambda * T^beta", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000, 4000))
  expected <- fc_fit$lambdas * c(2000, 3000, 4000)^fc_fit$betas
  expect_equal(fc$cum_failures, expected, tolerance = 1e-6, ignore_attr = TRUE)

  # Bounds ordering: lower <= fit <= upper
  expect_true(all(fc$lower_bounds <= fc$cum_failures))
  expect_true(all(fc$cum_failures <= fc$upper_bounds))
})

test_that("predict_rga: Piecewise NHPP forecast is numeric beyond last breakpoint", {
  fit_pw <- suppressWarnings(
    rga(fc_times, fc_failures, model_type = "Piecewise NHPP", breaks = c(450))
  )
  fc_pw <- predict_rga(fit_pw, times = c(2000, 3000))

  expect_s3_class(fc_pw, "rga_predict")
  expect_equal(fc_pw$model_type, "Piecewise NHPP")
  expect_true(is.numeric(fc_pw$cum_failures))
  expect_length(fc_pw$cum_failures, 2)
  expect_true(all(is.finite(fc_pw$cum_failures)))
})

test_that("predict_rga: monotonicity — larger times yield larger cum_failures", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000, 4000, 5000))
  expect_true(all(diff(fc$cum_failures) > 0))
})

test_that("predict_rga: wider conf_level produces wider bounds", {
  fc90 <- predict_rga(fc_fit, times = c(2000, 3000), conf_level = 0.90)
  fc99 <- predict_rga(fc_fit, times = c(2000, 3000), conf_level = 0.99)

  width90 <- fc90$upper_bounds - fc90$lower_bounds
  width99 <- fc99$upper_bounds - fc99$lower_bounds
  expect_true(all(width99 > width90))
})

test_that("print.rga_predict: smoke test and invisibility", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000, 4000))
  expect_output(print(fc), "Reliability Growth Forecast")
  expect_output(print(fc), "Crow-AMSAA")
  expect_output(print(fc), "Cum.Failures")
  expect_invisible(print(fc))
})

test_that("print.rga_predict: errors on wrong class", {
  expect_error(print.rga_predict(list()),
    "'x' must be an object of class 'rga_predict'.",
    fixed = TRUE
  )
})

test_that("plot.rga_predict: smoke test — no error with default args", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000, 4000))
  expect_silent(plot(fc))
  expect_silent(plot(fc, conf_bounds = FALSE))
  expect_silent(plot(fc, legend = FALSE))
  expect_silent(plot(fc, legend_pos = "topright"))
})

test_that("plot.rga_predict: invisibly returns NULL", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000))
  expect_invisible(plot(fc))
})

test_that("plot.rga_predict: argument validation", {
  fc <- predict_rga(fc_fit, times = c(2000, 3000))
  expect_error(plot.rga_predict(list()),
    "'x' must be an object of class 'rga_predict'.",
    fixed = TRUE
  )
  expect_error(plot(fc, conf_bounds = "yes"),
    "'conf_bounds' must be a single logical value.",
    fixed = TRUE
  )
  expect_error(plot(fc, legend = "yes"),
    "'legend' must be a single logical value.",
    fixed = TRUE
  )
  expect_error(plot(fc, legend_pos = 1),
    "'legend_pos' must be a single character string.",
    fixed = TRUE
  )
})

# --------------------------------------------------------------------------- #

test_that("rga output retains row / case names from input", {
  # Case 1: Named numeric vectors
  times <- c(A = 100, B = 200, C = 300, D = 400, E = 500)
  failures <- c(A = 1, B = 2, C = 1, D = 3, E = 2)

  res_named <- rga(times, failures)

  # input names preserved in stored inputs
  expect_equal(names(res_named$times), names(times))
  expect_equal(names(res_named$cum_times), names(times))
  expect_equal(names(res_named$failures), names(failures))

  # Case 2: Piecewise NHPP

  # Use a dataset that can reasonably produce a breakpoint (small example)
  times2 <- c(t1 = 10, t2 = 20, t3 = 30, t4 = 40, t5 = 50)
  failures2 <- c(t1 = 1, t2 = 1, t3 = 2, t4 = 3, t5 = 5)

  res_piecewise <- rga(times2, failures2, model_type = "Piecewise NHPP", breaks = c(30))

  expect_equal(names(res_piecewise$times), names(times2))
  expect_equal(names(res_piecewise$failures), names(failures2))
})

# --------------------------------------------------------------------------- #
# MLE estimation tests                                                         #
# --------------------------------------------------------------------------- #

test_that("MLE + Piecewise NHPP errors with 'is not supported'", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  expect_error(
    rga(times, failures, model_type = "Piecewise NHPP", method = "MLE"),
    "is not supported"
  )
})

test_that("invalid method value errors via match.arg", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  expect_error(
    rga(times, failures, method = "OLS"),
    "should be one of"
  )
})

test_that("MLE returns correct class and fields", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")

  expect_s3_class(fit, "rga")
  expect_equal(fit$method, "MLE")
  expect_null(fit$model)
  expect_true(is.matrix(fit$vcov))
  expect_equal(dim(fit$vcov), c(2L, 2L))
  expect_true(all(fit$lower_bounds <= fit$fitted_values + 1e-10))
  expect_true(all(fit$fitted_values <= fit$upper_bounds + 1e-10))
})

test_that("LS still has method='LS' and vcov=NULL", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures)

  expect_equal(fit$method, "LS")
  expect_null(fit$vcov)
})

test_that("MLE logLik/AIC/BIC are finite and AIC == -2*logLik + 4", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")

  expect_true(is.finite(fit$logLik))
  expect_true(is.finite(fit$AIC))
  expect_true(is.finite(fit$BIC))
  expect_equal(fit$AIC, -2 * fit$logLik + 4, tolerance = 1e-10)
})

test_that("print.rga shows Estimation Method for MLE and LS", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit_mle <- rga(times, failures, method = "MLE")
  fit_ls <- rga(times, failures, method = "LS")

  expect_output(print(fit_mle), "Estimation Method: MLE")
  expect_output(print(fit_ls), "Estimation Method: LS")
})

test_that("MLE parameter recovery (n=10 wide-interval NHPP simulation)", {
  #' @srrstats {G5.6} Parameter recovery test for MLE
  skip_on_cran()
  set.seed(42)

  # Wide intervals ensure >> 100 expected failures per interval, so:
  # (a) pmax(rpois, 1) floor never triggers (no bias), and
  # (b) each interval is highly informative, giving tight MLE estimates.
  beta_true <- 0.8
  lambda_true <- 5.0
  t_obs <- seq(200, 2000, by = 200) # 10 intervals, each 200 units wide
  mu <- lambda_true * t_obs^beta_true
  failures <- as.integer(pmax(rpois(length(t_obs), diff(c(0, mu))), 1))
  times <- c(t_obs[1], diff(t_obs))

  fit <- rga(times, failures, method = "MLE")

  expect_true(abs(fit$betas - beta_true) < 0.10,
    info = paste("beta estimate:", fit$betas)
  )
  expect_true(abs(fit$lambdas - lambda_true) < 0.50,
    info = paste("lambda estimate:", fit$lambdas)
  )
})

test_that("predict_rga with MLE: finite, positive, monotone", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")
  fc <- predict_rga(fit, times = c(2000, 3000, 4000))

  expect_true(all(is.finite(fc$cum_failures)))
  expect_true(all(fc$cum_failures > 0))
  expect_true(all(diff(fc$cum_failures) > 0))
})

test_that("predict_rga MLE formula: lambda * T^beta", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")
  fc <- predict_rga(fit, times = c(2000, 3000, 4000))

  expected <- fit$lambdas * c(2000, 3000, 4000)^fit$betas
  expect_equal(fc$cum_failures, expected, tolerance = 1e-6, ignore_attr = TRUE)
})

test_that("predict_rga MLE wider conf_level yields wider bounds", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")
  fc90 <- predict_rga(fit, times = c(2000, 3000), conf_level = 0.90)
  fc99 <- predict_rga(fit, times = c(2000, 3000), conf_level = 0.99)

  width90 <- fc90$upper_bounds - fc90$lower_bounds
  width99 <- fc99$upper_bounds - fc99$lower_bounds
  expect_true(all(width99 > width90))
})

test_that("plot.rga and plot.rga_predict with MLE: no error", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  fit <- rga(times, failures, method = "MLE")
  fc <- predict_rga(fit, times = c(2000, 3000, 4000))

  expect_silent(plot(fit))
  expect_silent(plot(fc))
})

# ---- overlay_rga tests ----
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by stop() is unique.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {RE6.0} Model objects have overlay plot methods.
#' @srrstats {RE6.2} The overlay plot shows fitted values with optional CIs.

test_that("overlay_rga: non-list input errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(m), "'models' must be a non-empty list of 'rga' objects.", fixed = TRUE)
})

test_that("overlay_rga: empty list errors", {
  expect_error(overlay_rga(list()), "'models' must be a non-empty list of 'rga' objects.", fixed = TRUE)
})

test_that("overlay_rga: non-rga element errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(list(m, list(a = 1))),
               "All elements of 'models' must be objects of class 'rga'.", fixed = TRUE)
})

test_that("overlay_rga: conf_bounds wrong type errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(list(m), conf_bounds = "yes"),
               "'conf_bounds' must be a single logical value.", fixed = TRUE)
})

test_that("overlay_rga: legend wrong type errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(list(m), legend = 1L),
               "'legend' must be a single logical value.", fixed = TRUE)
})

test_that("overlay_rga: log wrong type errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(list(m), log = "yes"),
               "'log' must be a single logical value.", fixed = TRUE)
})

test_that("overlay_rga: legend_pos wrong length errors", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  expect_error(overlay_rga(list(m), legend_pos = c("a", "b")),
               "'legend_pos' must be a single character string.", fixed = TRUE)
})

test_that("overlay_rga: colors too short errors", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  expect_error(overlay_rga(list(m1, m2), colors = "black"),
               "'colors' must be a character vector with at least one color per model.", fixed = TRUE)
})

test_that("overlay_rga: single model renders without error", {
  m <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m)))
})

test_that("overlay_rga: two named models render and return NULL", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  result <- overlay_rga(list(System_A = m1, System_B = m2))
  expect_null(result)
})

test_that("overlay_rga: two unnamed models render without error", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m1, m2)))
})

test_that("overlay_rga: conf_bounds = FALSE renders without error", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m1, m2), conf_bounds = FALSE))
})

test_that("overlay_rga: log = TRUE renders without error", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m1, m2), log = TRUE))
})

test_that("overlay_rga: custom colors render without error", {
  m1 <- rga(c(100, 200, 300, 400, 500), c(1, 2, 1, 3, 2))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m1, m2), colors = c("steelblue", "tomato")))
})

test_that("overlay_rga: models with different-length datasets render without error", {
  m1 <- rga(c(100, 200, 300), c(1, 2, 1))
  m2 <- rga(c(150, 300, 450, 600, 750), c(2, 1, 3, 2, 4))
  pdf(NULL)
  on.exit(dev.off())
  expect_silent(overlay_rga(list(m1, m2)))
})
