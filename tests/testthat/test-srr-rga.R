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
  expect_error(rga(df1),
               "must contain columns 'times' and 'failures'")

  df2 <- data.frame(times = 1:3)
  expect_error(rga(df2),
               "must contain columns 'times' and 'failures'")
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
               fixed = TRUE)
})

test_that("NA / NaN checks for times and failures", {
  expect_error(rga(c(1, NA), valid_failures),
               "'times' contains missing (NA) or NaN values.",
               fixed = TRUE)

  expect_error(rga(valid_times, c(1, NaN, 1)),
               "'failures' contains missing (NA) or NaN values.",
               fixed = TRUE)
})

test_that("type checks for times and failures", {
  expect_error(rga(list(1,2,3), valid_failures),
               "'times' must be a numeric vector.",
               fixed = TRUE)

  expect_error(rga(valid_times, list(1,2,3)),
               "'failures' must be a numeric vector.",
               fixed = TRUE)
})

test_that("empty vector checks", {
  expect_error(rga(numeric(0), numeric(0)),
               "'times' cannot be empty.",
               fixed = TRUE)

  # ensure failures empty triggers its message
  expect_error(rga(valid_times, numeric(0)),
               "'failures' cannot be empty.",
               fixed = TRUE)
})

test_that("length mismatch check", {
  expect_error(rga(c(1, 2), c(1, 2, 3)),
               "The length of 'times' and 'failures' must be equal.",
               fixed = TRUE)
})

test_that("finite and >0 checks for times and failures", {
  expect_error(rga(c(1, Inf, 3), c(1,1,1)),
               "All values in 'times' must be finite and > 0.",
               fixed = TRUE)

  expect_error(rga(c(1,2,3), c(1, 0, 2)),
               "All values in 'failures' must be finite and > 0.",
               fixed = TRUE)
})

test_that("model_type validation errors", {
  # non-single character
  expect_error(rga(valid_times, valid_failures, model_type = c("a", "b")),
               "'model_type' must be a single character string.",
               fixed = TRUE)

  # invalid string - match.arg produces its own message; check for 'one of'
  expect_error(rga(valid_times, valid_failures, model_type = "not-a-model"),
               "one of")
})

test_that("breaks argument validation", {
  # breaks must be numeric non-empty vector if provided
  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = character(1)),
               "'breaks' must be a non-empty numeric vector if provided.",
               fixed = TRUE)

  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = numeric(0)),
               "'breaks' must be a non-empty numeric vector if provided.",
               fixed = TRUE)

  # breaks values must be finite and > 0
  expect_error(rga(valid_times, valid_failures, model_type = "Piecewise NHPP", breaks = c(100, -1)),
               "All values in 'breaks' must be finite and > 0.",
               fixed = TRUE)

  # breaks only allowed with piecewise model
  expect_error(rga(valid_times, valid_failures, model_type = "Crow-AMSAA", breaks = c(150)),
               "'breaks' can only be used with the 'Piecewise NHPP' model.",
               fixed = TRUE)
})

test_that("conf_level validation", {
  expect_error(rga(valid_times, valid_failures, conf_level = c(0.9, 0.95)),
               "'conf_level' must be a single numeric value.",
               fixed = TRUE)

  expect_error(rga(valid_times, valid_failures, conf_level = 1),
               "'conf_level' must be between 0 and 1 (exclusive).",
               fixed = TRUE)

  expect_error(rga(valid_times, valid_failures, conf_level = 0),
               "'conf_level' must be between 0 and 1 (exclusive).",
               fixed = TRUE)
})

test_that("print.rga errors when input is not rga", {
  expect_error(print.rga(list()),
               "'x' must be an object of class 'rga'.",
               fixed = TRUE)
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
               fixed = TRUE)

  # conf_bounds must be single logical
  expect_error(plot.rga(x_good, conf_bounds = "nope"),
               "'conf_bounds' must be a single logical value.",
               fixed = TRUE)

  # legend must be single logical
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = "nope"),
               "'legend' must be a single logical value.",
               fixed = TRUE)

  # log must be single logical
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = TRUE, log = "nope"),
               "'log' must be a single logical value.",
               fixed = TRUE)

  # legend_pos must be single character string
  expect_error(plot.rga(x_good, conf_bounds = TRUE, legend = TRUE, log = FALSE, legend_pos = c("a","b")),
               "'legend_pos' must be a single character string.",
               fixed = TRUE)

  # malformed rga: missing required model$model columns
  x_bad_model <- list(model = list(model = data.frame(foo = 1, bar = 2)))
  class(x_bad_model) <- "rga"
  expect_error(plot.rga(x_bad_model),
               "The 'rga' object appears malformed or missing model data.",
               fixed = TRUE)
})

library(testthat)

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
              info = paste("beta estimate:", fit$betas))
  expect_true(abs(fit$lambdas - lambda_true) < tol_lambda,
              info = paste("lambda estimate:", fit$lambdas))
})

test_that("Piecewise NHPP parameter recovery works", {

  # True params
  beta1 <- 0.7
  beta2 <- 1.4
  lambda1 <- 0.02
  tb <- 200
  lambda2 <- lambda1 * tb^(beta1 - beta2)  # continuity at breakpoint

  # Generate observation times and interval inputs for rga()
  time_points <- seq(5, 1000, length.out = 200)
  intervals <- c(time_points[1], diff(time_points))

  # True cumulative counts (piecewise power law, continuous at tb)
  cumN <- ifelse(time_points <= tb,
                 lambda1 * (time_points ^ beta1),
                 lambda2 * (time_points ^ beta2))
  failures <- c(cumN[1], diff(cumN))
  stopifnot(all(failures > 0))

  # Run rga with the known breakpoint (simpler, direct parameter recovery)
  res <- rga(times = intervals, failures = failures,
             model_type = "Piecewise NHPP", breaks = tb, conf_level = 0.95)

  # Helper: extract numeric betas from rga output (handles usual segmented structure)
  get_betas_num <- function(b) {
    if (is.numeric(b)) return(as.numeric(b))
    if (is.list(b) && "log_times" %in% names(b)) {
      mat <- b$log_times
      if ("Est." %in% colnames(mat)) return(as.numeric(mat[, "Est."]))
      if ("Estimate" %in% colnames(mat)) return(as.numeric(mat[, "Estimate"]))
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
    expect_true(runtime < 5)  # fail if runtime > 5 sec
  }
})

test_that("Crow-AMSAA is robust to small noise in times", {
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

