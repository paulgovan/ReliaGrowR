# Internal MLE helper for Crow-AMSAA (NHPP power-law) grouped-data likelihood.
# Arguments:
#   cum_time   : numeric vector of cumulative times (cumsum of interval times)
#   failures   : numeric vector of failure counts per interval
#   conf_level : numeric scalar in (0,1)
# Returns a named list of estimates, standard errors, fitted values, CIs,
# residuals, and information criteria.
.fit_mle_crow <- function(cum_time, failures, conf_level) {
  N <- sum(failures)
  T_max <- max(cum_time)
  n_obs <- length(failures)
  t_prev <- c(0, cum_time[-n_obs])

  neg_loglik <- function(par) {
    beta <- par[1]
    lambda <- par[2]
    if (beta <= 0 || lambda <= 0) {
      return(Inf)
    }
    delta_t <- cum_time^beta - t_prev^beta
    if (any(delta_t <= 0)) {
      return(Inf)
    }
    ll <- N * log(lambda) + sum(failures * log(delta_t)) - lambda * T_max^beta
    -ll
  }

  opt <- stats::optim(
    par = c(1, N / T_max),
    fn = neg_loglik,
    method = "L-BFGS-B",
    lower = c(1e-6, 1e-10),
    hessian = TRUE
  )

  beta_hat <- opt$par[1]
  lambda_hat <- opt$par[2]
  loglik <- -opt$value

  vcov_mat <- tryCatch(solve(opt$hessian), error = function(e) matrix(NA_real_, 2, 2))
  beta_se <- if (!anyNA(vcov_mat)) sqrt(max(vcov_mat[1, 1], 0)) else NA_real_

  fitted_values <- lambda_hat * cum_time^beta_hat

  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)
  log_fit <- log(fitted_values)
  grad_mat <- cbind(log(cum_time), 1 / lambda_hat)
  var_lf <- rowSums((grad_mat %*% vcov_mat) * grad_mat)
  hw <- z_val * sqrt(pmax(var_lf, 0))

  list(
    beta          = beta_hat,
    lambda        = lambda_hat,
    betas_se      = beta_se,
    vcov          = vcov_mat,
    fitted_values = fitted_values,
    lower_bounds  = exp(log_fit - hw),
    upper_bounds  = exp(log_fit + hw),
    loglik        = loglik,
    residuals     = cumsum(failures) - fitted_values,
    aic           = -2 * loglik + 4,
    bic           = -2 * loglik + 2 * log(n_obs)
  )
}

.compute_rga_cum_times <- function(times, times_type) {
  if (identical(times_type, "failure_times")) {
    return(cumsum(times))
  }

  if (any(diff(times) <= 0)) {
    stop(
      "When 'times_type = \"cumulative_failure_times\"', 'times' must be strictly increasing."
    )
  }

  times
}

#' Reliability Growth Analysis.
#'
#' This function performs reliability growth analysis using the Crow-AMSAA model by
#' Crow (1975) (AMSAATR138) or piecewise
#' NHPP model by Guo et al. (2010) <doi:10.1109/RAMS.2010.5448029>. It fits
#' a log-log linear regression of cumulative failures versus cumulative time. The
#' function accepts either two numeric vectors (`times`, `failures`) or a data frame
#' containing both. The `Piecewise NHPP` model can automatically detect change points
#' or use user-specified breakpoints.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
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
#' @srrstats {RE1.2} Documentation includes expected format for inputting predictor variables (`times`, `failures`).
#' @srrstats {RE1.3} Output retains all relevant aspects of input data.
#' @srrstats {RE1.3a} Output retains all relevant aspects of input data.
#' @srrstats {RE1.4} Documentation includes assumptions for the input data (i.e., positive, finite values).
#' @srrstats {RE2.1} NA, NaN, and Inf values in input data results in an error.
#' @srrstats {RE2.2} Missing values in input data results in an error.
#' @srrstats {RE2.4} Function includes check for perfect collinearity between predictor and response variables.
#' @srrstats {RE2.4a} Perfect collinearity between predictor and response variables results in an error.
#' @srrstats {RE2.4b} Perfect collinearity between predictor and response variables results in an error.
#' @srrstats {RE4.0} Software returns a “model” object, which is a modified `lm` model object.
#' @srrstats {RE4.2} Model coefficients are included in the output object.
#' @srrstats {RE4.3} Standard errors on the coefficients are included in the output object.
#' @srrstats {RE4.4} The model specification is included in the output object.
#' @srrstats {RE4.5} The numbers of observations is included in the output object.
#' @srrstats {RE4.8} The Response variable (cumulative failures) is included in the output object.
#' @srrstats {RE4.9} Modeled values of the response variable are included in the output object.
#' @srrstats {RE4.10} Model Residuals, including documentation is included in the output object.
#' @srrstats {RE4.11} Goodness-of-fit statistics (log-likelihood, AIC, BIC) are included
#' in the output object.
#' @srrstats {RE4.13} All input variables are included in the output object.
#' @srrstats {RE4.17} Model objects are extended by a default `print` method which
#' provides an on-screen summary of model (input) parameters and (output) coefficients.
#' @srrstats {RE5.0} Scaling relationships between sizes of input data and
#' speed of algorithm are documented in the function documentation.
#' @srrstats {RE6.0} Model objects have default plot methods.
#' @srrstats {RE6.2} The default plot method produces a plot of the fitted values
#' of the model, with optional visualisation of confidence intervals.
#' @srrstats {RE7.1} Unit tests check for noiseless, exact relationships between
#' predictor (independent) and response (dependent) data.
#' @srrstats {RE7.1a} Unit tests confirm that model fitting is at least as fast
#' or faster than testing with equivalent noisy data.
#' @srrstats {RE7.2} Unit tests demonstrate that output objects retain aspects
#' of input data such as case names.
#' @srrstats {RE7.3} Unit tests demonstrate expected behavior when `rga` object
#' is submitted to the accessor methods `print` and `plot`.
#'
#' @param times Either a numeric vector of failure-time inputs or a data frame
#' containing both time inputs and failure counts. If `times_type = "failure_times"`
#' (default), `times` is treated exactly as in previous versions of the function
#' and is cumulatively summed inside `rga()`. If
#' `times_type = "cumulative_failure_times"`, `times` is treated as already
#' cumulative and is used directly without applying `cumsum()`. If a data frame
#' is provided, it must contain two columns: `times` and `failures`.
#' @param failures A numeric vector of the number of failures at each corresponding time
#' in times. Must be the same length as `times` if both are vectors. All values must be
#' positive and finite. Ignored if `times` is a data frame.
#' @param times_type Character scalar indicating how to interpret `times`.
#'   `"failure_times"` (default) preserves the current behavior and cumulatively
#'   sums `times` inside `rga()`. `"cumulative_failure_times"` treats `times`
#'   as already cumulative and skips that internal `cumsum()`.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise NHPP` with change point detection.
#' @param breaks An optional vector of breakpoints for the `Piecewise NHPP` model.
#' @param conf_level The desired confidence level, which defaults to 95%. The confidence
#' level is the probability that the confidence interval contains the true mean response.
#' @family Reliability Growth Analysis
#' @return The function returns an object of class `rga` that contains:
#' \item{times}{The input time vector, stored exactly as supplied.}
#' \item{cum_times}{The cumulative time vector used for fitting.}
#' \item{times_type}{How `times` was interpreted: `"failure_times"` or `"cumulative_failure_times"`.}
#' \item{failures}{The input number of failures.}
#' \item{n_obs}{The number of observations (failures).}
#' \item{cum_failures}{Cumulative failures.}
#' \item{model}{The fitted model object (lm (linear model) or segmented).}
#' \item{residuals}{Model residuals on the log-log scale. These represent deviations of the observed
#' log cumulative failures from the fitted values and are useful for diagnostic checking.}
#' \item{logLik}{The log-likelihood of the fitted model. The log-likelihood is a
#' measure of model fit, with higher values indicating a better fit.}
#' \item{AIC}{Akaike Information Criterion (AIC). AIC is a measure used for model selection,
#' with lower values indicating a better fit.}
#' \item{BIC}{Bayesian Information Criterion(BIC). BIC is another criterion for model selection}
#' \item{breakpoints}{Breakpoints (log scale) if applicable.}
#' \item{fitted_values}{Fitted cumulative failures on the original scale.}
#' \item{lower_bounds}{Lower confidence bounds (original scale).}
#' \item{upper_bounds}{Upper confidence bounds (original scale).}
#' \item{betas}{Estimated beta(s). Betas are the slopes of the log-log plot.}
#' \item{betas_se}{Standard error(s) of the estimated beta(s).}
#' \item{growth_rate}{Estimated growth rate(s). Growth rates are calculated as 1 - beta.}
#' \item{lambdas}{Estimated lambda(s). Lambdas are the intercepts of the log-log plot.}
#'
#' @details
#' The scaling relationship between the size of input data (numbers of observations)
#' and speed of algorithm execution is approximately linear (O(n)). The function is
#' efficient and can handle large data sets (e.g., thousands of observations) quickly.
#' The function uses the `segmented` package for piecewise regression, which employs
#' an iterative algorithm to estimate breakpoints. The number of iterations required
#' for convergence may vary depending on the data and initial values.
#' In practice, the function typically converges within a few iterations for most data sets.
#' However, in some cases, especially with complex data or poor initial values,
#' it may take more iterations.
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result1 <- rga(times, failures)
#' print(result1)
#'
#' df <- data.frame(times = times, failures = failures)
#' result2 <- rga(df)
#' print(result2)
#'
#' cum_times <- cumsum(times)
#' result2b <- rga(cum_times, failures, times_type = "cumulative_failure_times")
#' print(result2b)
#'
#' result3 <- rga(times, failures, model_type = "Piecewise NHPP")
#' print(result3)
#'
#' result4 <- rga(times, failures, model_type = "Piecewise NHPP", breaks = c(450))
#' print(result4)
#' @param method Estimation method: \code{"LS"} (default) for least-squares
#'   log-log regression, or \code{"MLE"} for maximum likelihood estimation of
#'   the Crow-AMSAA model. \code{"MLE"} is not supported for
#'   \code{model_type = "Piecewise NHPP"}.
#' @importFrom stats lm predict AIC BIC logLik cor residuals optim qnorm
#' @importFrom segmented segmented slope intercept seg.control
#' @export
rga <- function(times, failures, times_type = c("failure_times", "cumulative_failure_times"),
                model_type = "Crow-AMSAA", breaks = NULL,
                conf_level = 0.95, method = c("LS", "MLE")) {
  if (is.data.frame(times)) {
    if (!all(c("times", "failures") %in% names(times))) {
      stop("If a data frame is provided, it must contain columns 'times' and 'failures'.")
    }
    failures <- times$failures
    times <- times$times
  }

  # Validation checks
  if (!is.numeric(times) || !is.vector(times)) {
    stop("'times' must be a numeric vector.")
  }
  if (!is.numeric(failures) || !is.vector(failures)) {
    stop("'failures' must be a numeric vector.")
  }
  if (any(is.na(times)) || any(is.nan(times))) {
    stop("'times' contains missing (NA) or NaN values.")
  }
  if (any(is.na(failures)) || any(is.nan(failures))) {
    stop("'failures' contains missing (NA) or NaN values.")
  }
  if (length(times) == 0) stop("'times' cannot be empty.")
  if (length(failures) == 0) stop("'failures' cannot be empty.")
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(!is.finite(times)) || any(times <= 0)) {
    stop("All values in 'times' must be finite and > 0.")
  }
  if (any(!is.finite(failures)) || any(failures <= 0)) {
    stop("All values in 'failures' must be finite and > 0.")
  }

  times_type <- match.arg(times_type)

  if (!is.character(model_type) || length(model_type) != 1) {
    stop("'model_type' must be a single character string.")
  }
  valid_model <- match.arg(
    tolower(model_type),
    tolower(c("crow-amsaa", "piecewise nhpp"))
  )

  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) == 0) {
      stop("'breaks' must be a non-empty numeric vector if provided.")
    }
    if (any(!is.finite(breaks)) || any(breaks <= 0)) {
      stop("All values in 'breaks' must be finite and > 0.")
    }
    if (valid_model != "piecewise nhpp") {
      stop("'breaks' can only be used with the 'Piecewise NHPP' model.")
    }
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  method <- match.arg(method)
  if (method == "MLE" && valid_model == "piecewise nhpp") {
    stop("'method = \"MLE\"' is not supported for model_type = \"Piecewise NHPP\". Use method = \"LS\".")
  }

  # Data prep
  cum_failures <- cumsum(failures)
  cum_time <- .compute_rga_cum_times(times, times_type)
  log_times <- log(cum_time)
  log_cum_failures <- log(cum_failures)

  # Check for perfect collinearity
  cor_val <- suppressWarnings(stats::cor(log_times, log_cum_failures))
  if (is.na(cor_val) || abs(cor_val - 1) < .Machine$double.eps^0.5 ||
    abs(cor_val + 1) < .Machine$double.eps^0.5) {
    stop("Perfect collinearity detected between predictor ('log_times') and response ('log_cum_failures'). Regression cannot be performed.")
  }

  # MLE early return
  if (method == "MLE") {
    mle <- .fit_mle_crow(cum_time, failures, conf_level)
    result <- list(
      times         = times,
      cum_times     = cum_time,
      times_type    = times_type,
      failures      = failures,
      n_obs         = length(failures),
      cum_failures  = cum_failures,
      model         = NULL,
      residuals     = mle$residuals,
      logLik        = mle$loglik,
      AIC           = mle$aic,
      BIC           = mle$bic,
      breakpoints   = NULL,
      fitted_values = mle$fitted_values,
      lower_bounds  = mle$lower_bounds,
      upper_bounds  = mle$upper_bounds,
      growth_rate   = 1 - mle$beta,
      betas         = mle$beta,
      betas_se      = mle$betas_se,
      lambdas       = mle$lambda,
      method        = "MLE",
      vcov          = mle$vcov
    )
    class(result) <- "rga"
    return(result)
  }

  # Fit initial Crow-AMSAA model
  fit <- stats::lm(log_cum_failures ~ log_times)

  if (valid_model == "piecewise nhpp") {
    if (is.null(breaks)) {
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times)
      breakpoints <- updated_fit$psi[, "Est."]
    } else {
      breakpoints <- log(breaks)
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = breakpoints)
    }
    slopes <- segmented::slope(updated_fit)
    intercepts <- segmented::intercept(updated_fit)
    betas <- slopes
    growth_rates <- 1 - slopes$log_times[, "Est."]
    lambdas <- exp(intercepts$log_times)

    # Standard errors
    beta_se <- slopes$log_times[, "St.Err."]
  } else {
    updated_fit <- fit
    breakpoints <- NULL
    smry <- summary(updated_fit)
    slope <- smry$coefficients[2, ]
    intercept <- smry$coefficients[1, ]
    betas <- slope["Estimate"]
    growth_rates <- 1 - betas
    lambdas <- exp(intercept["Estimate"])

    # Standard Error
    beta_se <- slope["Std. Error"]
    lambdas_se <- exp(intercept["Std. Error"])
  }

  # Fit statistics
  loglik <- as.numeric(stats::logLik(updated_fit))
  aic <- stats::AIC(updated_fit)
  bic <- stats::BIC(updated_fit)

  # Predictions values
  fitted_values <- stats::predict(updated_fit)
  residuals <- stats::residuals(updated_fit)
  conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)
  lower_bounds <- exp(conf_intervals[, "lwr"])
  upper_bounds <- exp(conf_intervals[, "upr"])

  # Return object
  result <- list(
    times = times,
    cum_times = cum_time,
    times_type = times_type,
    failures = failures,
    n_obs = length(failures),
    cum_failures = cum_failures,
    model = updated_fit,
    residuals = residuals,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    breakpoints = breakpoints,
    fitted_values = exp(fitted_values),
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    growth_rate = growth_rates,
    betas = betas,
    betas_se = beta_se,
    lambdas = lambdas,
    method = "LS",
    vcov = NULL
  )
  class(result) <- "rga"
  return(result)
}

#' Print method for rga objects.
#'
#' This function prints a summary of the results from an object of class \code{rga}.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
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
#'
#' @param x An object of class \code{rga}, which contains the results from the RGA model.
#' @param ... Additional arguments (not used).
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#' @family Reliability Growth Analysis
#' @return Invisibly returns the input object.
#'
#' @export
print.rga <- function(x, ...) {
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }

  cat("Reliability Growth Analysis (RGA)\n")
  cat("---------------------------------\n")

  model_type <- if (is.null(x$breakpoints)) "Crow-AMSAA" else "Piecewise NHPP"
  cat("Model Type:", model_type, "\n")
  est_method <- if (!is.null(x$method)) x$method else "LS"
  cat("Estimation Method:", est_method, "\n\n")

  if (!is.null(x$breakpoints)) {
    cat("Breakpoints (original scale):\n")
    cat(round(exp(x$breakpoints), 4), "\n\n")
  }

  # Number of observations (failures)
  cat(sprintf("\nNumber of observations (failures): %d\n", x$n_obs))

  cat("Parameters (per segment):\n")
  if (model_type == "Piecewise NHPP") {
    growth_rates <- round(x$growth_rate, 4)
    betas <- round(x$betas$log_times[, "Est."], 4)
    betas_se <- round(x$betas_se, 4)
    cat(sprintf("  Growth Rates: %s\n", paste(growth_rates, collapse = ", ")))
    cat(sprintf("  Betas: %s\n", paste(betas, collapse = ", ")))
    cat(sprintf("  Std. Errors (Betas): %s\n", paste(betas_se, collapse = ", ")))

    lambdas <- round(x$lambdas[, "Est."], 4)
    cat(sprintf("  Lambdas: %s\n", paste(lambdas, collapse = ", ")))
  } else {
    cat(sprintf("  Growth Rate: %.4f\n", x$growth_rate))
    cat(sprintf("  Beta: %.4f (SE = %.4f)\n", x$betas, x$betas_se))
    cat(sprintf("  Lambda: %.4f\n", x$lambdas))
  }

  cat("\nGoodness of Fit:\n")
  cat(sprintf("  Log-likelihood: %.2f\n", x$logLik))
  cat(sprintf("  AIC: %.2f\n", x$AIC))
  cat(sprintf("  BIC: %.2f\n", x$BIC))

  invisible(x)
}

#' Plot Method for RGA Objects
#'
#' This function generates plots for objects of class \code{rga}.
#'
#' @srrstats {G1.0} Primary references for Crow-AMSAA and Piecewise NHPP models
#' are provided in the description.
#' @srrstats {G1.1} The `rga` function is the first implementation of the Crow-AMSAA
#'  and Piecewise NHPP models within an R package on CRAN.
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.3} See sub-tags for responses.
#' @srrstats {G2.3a} `match.arg()` is used for string inputs.
#' @srrstats {G2.3b} `tolower()` is used for string inputs.
#' @srrstats {G2.4b} Explicit conversion of log-likelihood to continuous is made via `as.numeric()`.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.7} Both one-dimensional vectors and data frames are accepted as input.
#' @srrstats {G2.8} Sub-functions `print.rga` and `plot.rga` are provided for the `rga` class.
#' @srrstats {G2.10} Data extracted from tabular `data.frame` objects are checked to ensure consistent behavior.
#' @srrstats {G2.11} Unit tests check that `data.frame` inputs  are appropriately processed and do not error without reason.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14} See sub-tags for responses.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if any are found.
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
#'
#' @param x An object of class \code{rga}, which contains the results from the RGA model.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param log Logical; use a log-log scale (default: FALSE).
#' @param legend_pos Position of the legend (default: "bottomright").
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Reliability Growth Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot(result,
#'   main = "Reliability Growth Analysis",
#'   xlab = "Cumulative Time", ylab = "Cumulative Failures"
#' )
#' @importFrom graphics lines abline legend plot
#' @export
plot.rga <- function(x,
                     conf_bounds = TRUE,
                     legend = TRUE,
                     log = FALSE,
                     legend_pos = "bottomright",
                     ...) {
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }
  if (!is.logical(conf_bounds) || length(conf_bounds) != 1) {
    stop("'conf_bounds' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.logical(log) || length(log) != 1) {
    stop("'log' must be a single logical value.")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string.")
  }

  if (!is.null(x$cum_times) && !is.null(x$cum_failures)) {
    times <- x$cum_times
    cum_failures <- x$cum_failures
  } else if (!is.null(x$method) && x$method == "MLE") {
    times <- cumsum(x$times)
    cum_failures <- cumsum(x$failures)
  } else {
    if (!all(c("log_times", "log_cum_failures") %in% names(x$model$model))) {
      stop("The 'rga' object appears malformed or missing model data.")
    }
    times <- exp(x$model$model$log_times)
    cum_failures <- exp(x$model$model$log_cum_failures)
  }

  # Base plot
  plot_args <- list(
    x = times,
    y = cum_failures,
    pch = 16,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # Fitted line
  graphics::lines(times, x$fitted_values, ...)

  # Confidence bounds
  if (conf_bounds) {
    graphics::lines(times, x$lower_bounds, lty = 2, ...)
    graphics::lines(times, x$upper_bounds, lty = 2, ...)
  }

  # Breakpoints
  if (!is.null(x$breakpoints)) {
    graphics::abline(v = exp(x$breakpoints), lty = 3)
  }

  # Legend
  if (legend) {
    legend_items <- list(
      labels = c("Observed", "Fitted Line"),
      cols = c("black", "black"),
      pch = c(16, NA),
      lty = c(NA, 1)
    )

    if (conf_bounds) {
      legend_items$labels <- c(legend_items$labels, "Confidence Bounds")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 2)
    }

    if (!is.null(x$breakpoints)) {
      legend_items$labels <- c(legend_items$labels, "Change Points")
      legend_items$cols <- c(legend_items$cols, "black")
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 3)
    }

    graphics::legend(
      legend_pos,
      legend = legend_items$labels,
      col = legend_items$cols,
      pch = legend_items$pch,
      lty = legend_items$lty,
      bty = "n"
    )
  }

  invisible(NULL)
}

#' Overlay Plot for Multiple RGA Models
#'
#' Plots multiple fitted \code{rga} objects on a single set of axes, using
#' distinct colors per model. Observed data points, fitted lines, and optional
#' confidence bounds are drawn for every model. Models may have been fit to
#' different datasets.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} List input is validated to contain only \code{rga} objects.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {RE6.0} Model objects have default plot methods.
#' @srrstats {RE6.2} The overlay plot shows fitted values with optional CIs.
#'
#' @param models A named or unnamed list of objects of class \code{rga}.
#'   At least one model must be provided. If the list is named, those names
#'   are used as legend labels; otherwise labels default to
#'   \code{"Model 1"}, \code{"Model 2"}, etc.
#' @param conf_bounds Logical; draw confidence bounds for each model
#'   (default: \code{TRUE}).
#' @param legend Logical; draw a legend (default: \code{TRUE}).
#' @param legend_pos Legend position keyword (default: \code{"bottomright"}).
#' @param colors Optional character vector of colors, one per model. If
#'   \code{NULL} (default), \code{palette()} colors are cycled.
#' @param log Logical; use log-log axes (default: \code{FALSE}).
#' @param ... Additional arguments passed to the initial \code{plot()} call
#'   (e.g., \code{main}, \code{xlab}, \code{ylab}). Not forwarded to subsequent
#'   \code{lines()} or \code{points()} calls.
#' @family Reliability Growth Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' t1 <- c(100, 200, 300, 400, 500)
#' f1 <- c(1, 2, 1, 3, 2)
#' t2 <- c(150, 300, 450, 600, 750)
#' f2 <- c(2, 1, 3, 2, 4)
#' m1 <- rga(t1, f1)
#' m2 <- rga(t2, f2)
#' overlay_rga(list(System_A = m1, System_B = m2),
#'   main = "RGA Overlay", xlab = "Cumulative Time",
#'   ylab = "Cumulative Failures"
#' )
#' @importFrom graphics plot points lines abline legend
#' @importFrom grDevices palette
#' @export
overlay_rga <- function(models,
                        conf_bounds = TRUE,
                        legend = TRUE,
                        legend_pos = "bottomright",
                        colors = NULL,
                        log = FALSE,
                        ...) {
  # Input validation
  if (!identical(class(models), "list") || length(models) == 0) {
    stop("'models' must be a non-empty list of 'rga' objects.")
  }
  not_rga <- !vapply(models, inherits, logical(1), what = "rga")
  if (any(not_rga)) {
    stop("All elements of 'models' must be objects of class 'rga'.")
  }
  if (!is.logical(conf_bounds) || length(conf_bounds) != 1) {
    stop("'conf_bounds' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.logical(log) || length(log) != 1) {
    stop("'log' must be a single logical value.")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string.")
  }

  n_models <- length(models)

  # Resolve model names
  mod_names <- names(models)
  if (is.null(mod_names) || any(mod_names == "")) {
    mod_names <- paste0("Model ", seq_len(n_models))
  }

  # Resolve colors
  if (is.null(colors)) {
    pal <- grDevices::palette()
    colors <- pal[((seq_len(n_models) - 1L) %% length(pal)) + 1L]
  } else {
    if (!is.character(colors) || length(colors) < n_models) {
      stop("'colors' must be a character vector with at least one color per model.")
    }
    colors <- colors[seq_len(n_models)]
  }

  # Extract observed x/y from each model
  extract_xy <- function(x) {
    if (!is.null(x$cum_times) && !is.null(x$cum_failures)) {
      list(times = x$cum_times, cum_failures = x$cum_failures)
    } else if (!is.null(x$method) && x$method == "MLE") {
      list(times = cumsum(x$times), cum_failures = cumsum(x$failures))
    } else {
      list(
        times        = exp(x$model$model$log_times),
        cum_failures = exp(x$model$model$log_cum_failures)
      )
    }
  }
  xy_list <- lapply(models, extract_xy)

  # Compute global axis limits
  all_x <- unlist(lapply(xy_list, `[[`, "times"))
  all_y <- c(
    unlist(lapply(xy_list, `[[`, "cum_failures")),
    unlist(lapply(models, `[[`, "fitted_values"))
  )
  if (conf_bounds) {
    all_y <- c(
      all_y,
      unlist(lapply(models, `[[`, "lower_bounds")),
      unlist(lapply(models, `[[`, "upper_bounds"))
    )
  }
  xlim <- range(all_x, finite = TRUE)
  ylim <- range(all_y, finite = TRUE)
  if (log) ylim[1] <- max(ylim[1], .Machine$double.eps)

  # Base plot using first model's observed data
  plot_args <- list(
    x    = xy_list[[1]]$times,
    y    = xy_list[[1]]$cum_failures,
    xlim = xlim,
    ylim = ylim,
    col  = colors[[1]],
    pch  = 16,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # Add remaining models' observed points
  if (n_models > 1L) {
    for (i in seq(2L, n_models)) {
      graphics::points(xy_list[[i]]$times, xy_list[[i]]$cum_failures,
                       pch = 16, col = colors[[i]])
    }
  }

  # Fitted lines, confidence bounds, and breakpoints for all models
  for (i in seq_len(n_models)) {
    xy  <- xy_list[[i]]
    mdl <- models[[i]]
    col <- colors[[i]]

    graphics::lines(xy$times, mdl$fitted_values, col = col, lty = 1)

    if (conf_bounds) {
      graphics::lines(xy$times, mdl$lower_bounds, col = col, lty = 2)
      graphics::lines(xy$times, mdl$upper_bounds, col = col, lty = 2)
    }

    if (!is.null(mdl$breakpoints)) {
      graphics::abline(v = exp(mdl$breakpoints), col = col, lty = 3)
    }
  }

  # Legend
  if (legend) {
    graphics::legend(
      legend_pos,
      legend = mod_names,
      col    = colors,
      pch    = 16,
      lty    = 1,
      bty    = "n"
    )
  }

  invisible(NULL)
}

#' Forecast Cumulative Failures from a Reliability Growth Model
#'
#' Takes a fitted \code{rga} object and a vector of cumulative times, returning
#' predicted cumulative failures with confidence bounds as an \code{rga_predict}
#' S3 object.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.6} One-dimensional inputs are appropriately pre-processed.
#' @srrstats {G2.8} Sub-functions \code{print.rga_predict} and
#'   \code{plot.rga_predict} are provided for the \code{rga_predict} class.
#' @srrstats {G2.13} The function checks for missing data and errors if any is found.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.14b} Missing data results in an error.
#' @srrstats {G2.14c} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any is found.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results
#'   with expected values.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results
#'   with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical
#'   algorithms produce expected results to fixed test data sets.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of
#'   the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected
#'   stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does
#'   not meaningfully change results.
#'
#' @param object An object of class \code{rga} returned by \code{rga()}.
#' @param times A numeric vector of cumulative times at which to forecast.
#'   All values must be finite and > 0. A warning is issued if any value is
#'   at or below the maximum observed cumulative time (hindcasting).
#' @param conf_level The desired confidence level (default \code{0.95}). Must
#'   be a single finite numeric in (0, 1).
#' @family Reliability Growth Analysis
#' @return An object of class \code{rga_predict} containing:
#' \item{times}{The forecast cumulative times.}
#' \item{cum_failures}{Predicted cumulative failures.}
#' \item{lower_bounds}{Lower confidence bounds.}
#' \item{upper_bounds}{Upper confidence bounds.}
#' \item{conf_level}{The confidence level used.}
#' \item{model_type}{Either \code{"Crow-AMSAA"} or \code{"Piecewise NHPP"}.}
#' \item{rga_object}{The original \code{rga} object (used by the plot method).}
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' fc <- predict_rga(fit, times = c(1500, 2000))
#' print(fc)
#' @export
predict_rga <- function(object, times, conf_level = 0.95) {
  if (!inherits(object, "rga")) {
    stop("'object' must be an object of class 'rga'.")
  }
  if (!is.numeric(times) || !is.vector(times)) {
    stop("'times' must be a numeric vector.")
  }
  if (length(times) == 0) {
    stop("'times' cannot be empty.")
  }
  if (any(is.na(times)) || any(is.nan(times))) {
    stop("'times' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(times)) || any(times <= 0)) {
    stop("All values in 'times' must be finite and > 0.")
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  if (!is.null(object$cum_times)) {
    max_obs_time <- max(object$cum_times)
  } else if (!is.null(object$model)) {
    max_obs_time <- max(exp(object$model$model$log_times))
  } else {
    max_obs_time <- max(cumsum(object$times))
  }
  if (any(times <= max_obs_time)) {
    warning(
      "Some 'times' values are <= the maximum observed cumulative time. ",
      "Hindcasting is allowed but may not be meaningful."
    )
  }

  model_type <- if (is.null(object$breakpoints)) "Crow-AMSAA" else "Piecewise NHPP"

  if (!is.null(object$method) && object$method == "MLE") {
    cum_failures <- object$lambdas * times^object$betas
    log_fitted <- log(cum_failures)
    z_val <- stats::qnorm(1 - (1 - conf_level) / 2)
    grad_mat <- cbind(log(times), 1 / object$lambdas)
    var_lf <- rowSums((grad_mat %*% object$vcov) * grad_mat)
    hw <- z_val * sqrt(pmax(var_lf, 0))
    lower_bounds <- exp(log_fitted - hw)
    upper_bounds <- exp(log_fitted + hw)
  } else {
    newdata <- data.frame(log_times = log(times))
    pred <- stats::predict(object$model,
      newdata = newdata,
      interval = "confidence", level = conf_level
    )
    cum_failures <- exp(pred[, "fit"])
    lower_bounds <- exp(pred[, "lwr"])
    upper_bounds <- exp(pred[, "upr"])
  }

  result <- list(
    times        = times,
    cum_failures = cum_failures,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    conf_level   = conf_level,
    model_type   = model_type,
    rga_object   = object
  )
  class(result) <- "rga_predict"
  result
}

#' Print Method for rga_predict Objects
#'
#' Prints a formatted table of forecast cumulative failures with confidence
#' bounds for an \code{rga_predict} object.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.8} This method is provided for the \code{rga_predict} class.
#' @srrstats {G5.2} Unit tests demonstrate output content.
#' @srrstats {G5.2b} Unit tests compare output with expected values.
#'
#' @param x An object of class \code{rga_predict}.
#' @param ... Additional arguments (not used).
#' @family Reliability Growth Analysis
#' @return Invisibly returns the input object.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' fc <- predict_rga(fit, times = c(1500, 2000))
#' print(fc)
#' @export
print.rga_predict <- function(x, ...) {
  if (!inherits(x, "rga_predict")) {
    stop("'x' must be an object of class 'rga_predict'.")
  }

  pct <- round(x$conf_level * 100)
  header <- sprintf("Reliability Growth Forecast (%s)", x$model_type)
  cat(header, "\n")
  cat(paste(rep("-", nchar(header) + 1), collapse = ""), "\n")

  df <- data.frame(
    Time           = x$times,
    Cum.Failures   = round(x$cum_failures, 1),
    Lower          = round(x$lower_bounds, 1),
    Upper          = round(x$upper_bounds, 1),
    check.names    = FALSE
  )
  names(df)[3] <- sprintf("Lower (%d%%)", pct)
  names(df)[4] <- sprintf("Upper (%d%%)", pct)

  print(df, row.names = FALSE)

  invisible(x)
}

#' Plot Method for rga_predict Objects
#'
#' Plots observed data, the fitted reliability growth curve, and the forecast
#' with optional confidence bounds for an \code{rga_predict} object.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.8} This method is provided for the \code{rga_predict} class.
#' @srrstats {G5.2} Unit tests include smoke tests for this method.
#'
#' @param x An object of class \code{rga_predict}.
#' @param conf_bounds Logical; include confidence bounds (default: \code{TRUE}).
#' @param legend Logical; show the legend (default: \code{TRUE}).
#' @param legend_pos Position of the legend (default: \code{"bottomright"}).
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Reliability Growth Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' fc <- predict_rga(fit, times = c(1500, 2000))
#' plot(fc, main = "RGA Forecast", xlab = "Cumulative Time", ylab = "Cumulative Failures")
#' @export
plot.rga_predict <- function(x,
                             conf_bounds = TRUE,
                             legend = TRUE,
                             legend_pos = "bottomright",
                             ...) {
  if (!inherits(x, "rga_predict")) {
    stop("'x' must be an object of class 'rga_predict'.")
  }
  if (!is.logical(conf_bounds) || length(conf_bounds) != 1) {
    stop("'conf_bounds' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string.")
  }

  rga_obj <- x$rga_object
  if (!is.null(rga_obj$cum_times) && !is.null(rga_obj$cum_failures)) {
    obs_times <- rga_obj$cum_times
    obs_cum_failures <- rga_obj$cum_failures
  } else if (!is.null(rga_obj$method) && rga_obj$method == "MLE") {
    obs_times <- cumsum(rga_obj$times)
    obs_cum_failures <- cumsum(rga_obj$failures)
  } else {
    obs_times <- exp(rga_obj$model$model$log_times)
    obs_cum_failures <- exp(rga_obj$model$model$log_cum_failures)
  }

  all_y <- c(obs_cum_failures, rga_obj$fitted_values, x$cum_failures)
  if (conf_bounds) {
    all_y <- c(
      all_y, rga_obj$lower_bounds, rga_obj$upper_bounds,
      x$lower_bounds, x$upper_bounds
    )
  }
  xlim <- range(c(obs_times, x$times))
  ylim <- range(all_y)

  graphics::plot(obs_times, obs_cum_failures,
    xlim = xlim, ylim = ylim, pch = 16, ...
  )

  graphics::lines(obs_times, rga_obj$fitted_values)

  if (conf_bounds) {
    graphics::lines(obs_times, rga_obj$lower_bounds, lty = 3)
    graphics::lines(obs_times, rga_obj$upper_bounds, lty = 3)
  }

  max_obs_time <- max(obs_times)
  graphics::abline(v = max_obs_time, lty = 2, col = "gray")

  graphics::lines(x$times, x$cum_failures, lty = 2)

  if (conf_bounds) {
    graphics::lines(x$times, x$lower_bounds, lty = 3)
    graphics::lines(x$times, x$upper_bounds, lty = 3)
  }

  if (legend) {
    pct <- round(x$conf_level * 100)
    legend_labels <- c("Observed", "Fitted", "Forecast")
    legend_pch <- c(16, NA, NA)
    legend_lty <- c(NA, 1, 2)
    legend_cols <- c("black", "black", "black")

    if (conf_bounds) {
      legend_labels <- c(legend_labels, sprintf("Conf. Bounds (%d%%)", pct))
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 3)
      legend_cols <- c(legend_cols, "black")
    }

    graphics::legend(legend_pos,
      legend = legend_labels,
      pch = legend_pch,
      lty = legend_lty,
      col = legend_cols,
      bty = "n",
      cex = 0.85,
      y.intersp = 1.3,
      x.intersp = 0.8
    )
  }

  invisible(NULL)
}
