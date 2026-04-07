# Internal MLE helper for Power Law NHPP.
# Fits N(t) = lambda * t^beta using maximum likelihood.
# Arguments:
#   cum_time   : numeric vector of cumulative event times
#   events     : numeric vector of event counts per time
#   conf_level : numeric scalar in (0,1)
# Returns a named list of estimates, SEs, fitted values, CIs, and GOF stats.
.fit_mle_power <- function(cum_time, events, conf_level) {
  N <- sum(events)
  T_max <- max(cum_time)
  n_obs <- length(events)
  t_prev <- c(0, cum_time[-n_obs])

  neg_loglik <- function(par) {
    beta <- par[1]
    lambda <- par[2]
    if (beta <= 0 || lambda <= 0) return(Inf)
    delta_t <- cum_time^beta - t_prev^beta
    if (any(delta_t <= 0)) return(Inf)
    ll <- N * log(lambda) + sum(events * log(delta_t)) - lambda * T_max^beta
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
    beta_se       = beta_se,
    vcov          = vcov_mat,
    fitted_values = fitted_values,
    lower_bounds  = exp(log_fit - hw),
    upper_bounds  = exp(log_fit + hw),
    loglik        = loglik,
    residuals     = cumsum(events) - fitted_values,
    aic           = -2 * loglik + 4,
    bic           = -2 * loglik + 2 * log(n_obs)
  )
}

# Internal MLE helper for Log-Linear NHPP.
# Fits intensity lambda(t) = exp(a + b*t), cumulative Lambda(t) = (exp(a)/b)(exp(bt) - 1).
# For grouped data with event counts at cumulative times.
.fit_mle_loglinear <- function(cum_time, events, conf_level) {
  N <- sum(events)
  T_max <- max(cum_time)
  n_obs <- length(events)
  t_prev <- c(0, cum_time[-n_obs])

  neg_loglik <- function(par) {
    a <- par[1]
    b <- par[2]
    if (abs(b) < 1e-12) {
      # HPP case: intensity = exp(a), cumulative = exp(a)*t
      delta_Lambda <- exp(a) * (cum_time - t_prev)
      if (any(!is.finite(delta_Lambda)) || any(delta_Lambda <= 0)) return(1e30)
      ll <- sum(events * log(delta_Lambda)) - exp(a) * T_max
    } else {
      # Log-linear: Lambda(t) = (exp(a)/b)(exp(bt) - 1)
      bt_curr <- b * cum_time
      bt_prev <- b * t_prev
      # Guard against overflow
      if (any(abs(bt_curr) > 500) || any(abs(bt_prev) > 500)) return(1e30)
      Lambda_curr <- (exp(a) / b) * (exp(bt_curr) - 1)
      Lambda_prev <- (exp(a) / b) * (exp(bt_prev) - 1)
      delta_Lambda <- Lambda_curr - Lambda_prev
      if (any(!is.finite(delta_Lambda)) || any(delta_Lambda <= 0)) return(1e30)
      total_Lambda <- (exp(a) / b) * (exp(b * T_max) - 1)
      if (!is.finite(total_Lambda)) return(1e30)
      ll <- sum(events * log(delta_Lambda)) - total_Lambda
    }
    if (!is.finite(ll)) return(1e30)
    -ll
  }

  # Use Nelder-Mead first to find a robust starting point
  opt_nm <- stats::optim(
    par = c(log(N / T_max), 1e-4),
    fn = neg_loglik,
    method = "Nelder-Mead"
  )

  opt <- stats::optim(
    par = opt_nm$par,
    fn = neg_loglik,
    method = "L-BFGS-B",
    lower = c(-20, -5),
    upper = c(20, 5),
    hessian = TRUE
  )

  a_hat <- opt$par[1]
  b_hat <- opt$par[2]
  loglik <- -opt$value

  vcov_mat <- tryCatch(solve(opt$hessian), error = function(e) matrix(NA_real_, 2, 2))
  a_se <- if (!anyNA(vcov_mat)) sqrt(max(vcov_mat[1, 1], 0)) else NA_real_
  b_se <- if (!anyNA(vcov_mat)) sqrt(max(vcov_mat[2, 2], 0)) else NA_real_

  # Fitted values: Lambda(t) = (exp(a)/b)(exp(bt) - 1)
  if (abs(b_hat) < 1e-12) {
    fitted_values <- exp(a_hat) * cum_time
  } else {
    fitted_values <- (exp(a_hat) / b_hat) * (exp(b_hat * cum_time) - 1)
  }

  # Delta method for confidence intervals on log(Lambda)
  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)
  log_fit <- log(pmax(fitted_values, 1e-300))

  # Gradient of log(Lambda) w.r.t. (a, b)
  if (abs(b_hat) < 1e-12) {
    grad_a <- rep(1, n_obs)
    grad_b <- cum_time / 2
  } else {
    # d/da log(Lambda) = 1
    grad_a <- rep(1, n_obs)
    # d/db log(Lambda) = (t*exp(bt))/(exp(bt)-1) - 1/b
    ebt <- exp(b_hat * cum_time)
    grad_b <- (cum_time * ebt) / (ebt - 1) - 1 / b_hat
  }
  grad_mat <- cbind(grad_a, grad_b)
  var_lf <- rowSums((grad_mat %*% vcov_mat) * grad_mat)
  hw <- z_val * sqrt(pmax(var_lf, 0))

  list(
    a             = a_hat,
    b             = b_hat,
    a_se          = a_se,
    b_se          = b_se,
    vcov          = vcov_mat,
    fitted_values = fitted_values,
    lower_bounds  = exp(log_fit - hw),
    upper_bounds  = exp(log_fit + hw),
    loglik        = loglik,
    residuals     = cumsum(events) - fitted_values,
    aic           = -2 * loglik + 4,
    bic           = -2 * loglik + 2 * log(n_obs)
  )
}


#' Non-Homogeneous Poisson Process Model for Repairable Systems.
#'
#' Fits a parametric NHPP model to recurrent event data from repairable systems.
#' Supported models include the Power Law process and the Log-Linear process.
#' The Power Law model can also be fit as a piecewise (segmented) model with
#' automatic change point detection or user-specified breakpoints.
#'
#' @srrstats {G1.0} The Power Law NHPP is described in Crow (1975), and the
#'   Log-Linear NHPP is described in Cox and Lewis (1966).
#' @srrstats {G1.3} All statistical terminology is explicitly defined.
#' @srrstats {G1.4} \code{roxygen2} documentation is used.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.3a} \code{match.arg()} is used for string inputs.
#' @srrstats {G2.7} Both vectors and data frames are accepted as input.
#' @srrstats {G2.13} The function checks for missing data.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.16} NA and NaN values result in an error.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#' @srrstats {RE2.1} NA, NaN, and Inf values in input result in an error.
#' @srrstats {RE4.0} Software returns a model object.
#' @srrstats {RE4.2} Model coefficients are included in the output.
#' @srrstats {RE4.11} Goodness-of-fit statistics are included.
#'
#' @param time A numeric vector of cumulative event times, or a data frame
#'   containing columns \code{time} and optionally \code{event}. All values
#'   must be positive, finite, and strictly increasing.
#' @param event An optional numeric vector of event counts at each time. If
#'   \code{NULL} (default), each time is treated as a single event.
#' @param data An optional data frame containing columns \code{time} and
#'   optionally \code{event}.
#' @param model_type Model type: \code{"Power Law"} (default) or
#'   \code{"Log-Linear"}.
#' @param breaks Optional vector of breakpoints for piecewise Power Law model.
#' @param method Estimation method: \code{"MLE"} (default) or \code{"LS"}.
#'   \code{"LS"} is not supported for \code{"Log-Linear"} models.
#' @param conf_level Confidence level for bounds (default 0.95).
#' @family Repairable Systems Analysis
#' @return An object of class \code{nhpp} containing:
#' \item{time}{The input cumulative event times.}
#' \item{event}{The event counts.}
#' \item{cum_events}{Cumulative event counts.}
#' \item{n_obs}{Number of observations.}
#' \item{model}{Fitted model object (lm or segmented), or NULL for MLE.}
#' \item{model_type}{\code{"Power Law"} or \code{"Log-Linear"}.}
#' \item{method}{\code{"MLE"} or \code{"LS"}.}
#' \item{params}{Named list of estimated parameters.}
#' \item{params_se}{Named list of standard errors.}
#' \item{vcov}{Variance-covariance matrix (MLE only).}
#' \item{fitted_values}{Fitted cumulative events.}
#' \item{lower_bounds}{Lower confidence bounds.}
#' \item{upper_bounds}{Upper confidence bounds.}
#' \item{residuals}{Model residuals.}
#' \item{logLik}{Log-likelihood.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{breakpoints}{Breakpoints (log scale) if piecewise model.}
#' \item{conf_level}{Confidence level used.}
#'
#' @details
#' The **Power Law NHPP** models the cumulative number of events as
#' \eqn{N(t) = \lambda t^\beta}{N(t) = lambda * t^beta}. The parameter
#' \eqn{\beta > 1} indicates a deteriorating system (increasing event rate),
#' \eqn{\beta < 1} an improving system, and \eqn{\beta = 1} a constant rate
#' (HPP).
#'
#' The **Log-Linear NHPP** models the intensity as
#' \eqn{\lambda(t) = \exp(a + bt)}{lambda(t) = exp(a + b*t)} with cumulative
#' function \eqn{\Lambda(t) = \frac{e^a}{b}(e^{bt} - 1)}{Lambda(t) = (exp(a)/b)(exp(bt) - 1)}.
#'
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' result <- nhpp(time, event)
#' print(result)
#' plot(result, main = "Power Law NHPP")
#'
#' result_ll <- nhpp(time, event, model_type = "Log-Linear")
#' print(result_ll)
#' @importFrom stats lm predict AIC BIC logLik cor residuals optim qnorm
#' @importFrom segmented segmented slope intercept seg.control
#' @export
nhpp <- function(time, event = NULL, data = NULL,
                 model_type = "Power Law",
                 breaks = NULL,
                 method = c("MLE", "LS"),
                 conf_level = 0.95) {

  # Extract from data frame
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame.")
    }
    if (!"time" %in% names(data)) {
      stop("'data' must contain a column named 'time'.")
    }
    time <- data$time
    if ("event" %in% names(data)) {
      event <- data$event
    }
  }

  # Validate time
  if (!is.numeric(time) || !is.vector(time)) {
    stop("'time' must be a numeric vector.")
  }
  if (length(time) == 0) stop("'time' cannot be empty.")
  if (any(is.na(time)) || any(is.nan(time))) {
    stop("'time' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop("All values in 'time' must be finite and > 0.")
  }
  if (is.unsorted(time, strictly = TRUE)) {
    stop("'time' must be strictly increasing.")
  }

  # Validate event
  if (is.null(event)) {
    event <- rep(1, length(time))
  }
  if (!is.numeric(event) || !is.vector(event)) {
    stop("'event' must be a numeric vector.")
  }
  if (length(event) != length(time)) {
    stop("'event' and 'time' must have the same length.")
  }
  if (any(is.na(event)) || any(is.nan(event))) {
    stop("'event' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(event)) || any(event <= 0)) {
    stop("All values in 'event' must be finite and > 0.")
  }

  # Validate model_type
  if (!is.character(model_type) || length(model_type) != 1) {
    stop("'model_type' must be a single character string.")
  }
  valid_model <- match.arg(
    tolower(model_type),
    tolower(c("power law", "log-linear"))
  )

  method <- match.arg(method)

  # Validate method/model combinations
  if (method == "LS" && valid_model == "log-linear") {
    stop("'method = \"LS\"' is not supported for model_type = \"Log-Linear\". Use method = \"MLE\".")
  }
  if (method == "MLE" && valid_model == "power law" && !is.null(breaks)) {
    stop("'method = \"MLE\"' is not supported for piecewise Power Law models. Use method = \"LS\".")
  }

  # Validate breaks
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || length(breaks) == 0) {
      stop("'breaks' must be a non-empty numeric vector if provided.")
    }
    if (any(!is.finite(breaks)) || any(breaks <= 0)) {
      stop("All values in 'breaks' must be finite and > 0.")
    }
    if (valid_model != "power law") {
      stop("'breaks' can only be used with the 'Power Law' model.")
    }
  }

  # Validate conf_level
  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  cum_events <- cumsum(event)
  n_obs <- length(event)

  # ---- Log-Linear MLE ----
  if (valid_model == "log-linear") {
    mle <- .fit_mle_loglinear(time, event, conf_level)
    result <- list(
      time          = time,
      event         = event,
      cum_events    = cum_events,
      n_obs         = n_obs,
      model         = NULL,
      model_type    = "Log-Linear",
      method        = "MLE",
      params        = list(a = mle$a, b = mle$b),
      params_se     = list(a_se = mle$a_se, b_se = mle$b_se),
      vcov          = mle$vcov,
      fitted_values = mle$fitted_values,
      lower_bounds  = mle$lower_bounds,
      upper_bounds  = mle$upper_bounds,
      residuals     = mle$residuals,
      logLik        = mle$loglik,
      AIC           = mle$aic,
      BIC           = mle$bic,
      breakpoints   = NULL,
      conf_level    = conf_level
    )
    class(result) <- "nhpp"
    return(result)
  }

  # ---- Power Law ----
  log_times <- log(time)
  log_cum_events <- log(cum_events)

  if (method == "MLE") {
    mle <- .fit_mle_power(time, event, conf_level)
    result <- list(
      time          = time,
      event         = event,
      cum_events    = cum_events,
      n_obs         = n_obs,
      model         = NULL,
      model_type    = "Power Law",
      method        = "MLE",
      params        = list(beta = mle$beta, lambda = mle$lambda),
      params_se     = list(beta_se = mle$beta_se),
      vcov          = mle$vcov,
      fitted_values = mle$fitted_values,
      lower_bounds  = mle$lower_bounds,
      upper_bounds  = mle$upper_bounds,
      residuals     = mle$residuals,
      logLik        = mle$loglik,
      AIC           = mle$aic,
      BIC           = mle$bic,
      breakpoints   = NULL,
      conf_level    = conf_level
    )
    class(result) <- "nhpp"
    return(result)
  }

  # ---- Power Law LS ----
  # Check for perfect collinearity
  cor_val <- suppressWarnings(stats::cor(log_times, log_cum_events))
  if (is.na(cor_val) || abs(cor_val - 1) < .Machine$double.eps^0.5 ||
    abs(cor_val + 1) < .Machine$double.eps^0.5) {
    stop("Perfect collinearity detected between 'log(time)' and 'log(cum_events)'. Regression cannot be performed.")
  }

  fit <- stats::lm(log_cum_events ~ log_times)

  if (!is.null(breaks)) {
    # Piecewise model
    breakpoints_log <- log(breaks)
    updated_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = breakpoints_log)

    slopes <- segmented::slope(updated_fit)
    intercepts <- segmented::intercept(updated_fit)
    betas <- slopes$log_times[, "Est."]
    beta_se <- slopes$log_times[, "St.Err."]
    lambdas <- exp(intercepts$log_times[, "Est."])

    params <- list(betas = betas, lambdas = lambdas)
    params_se <- list(betas_se = beta_se)
    bp <- updated_fit$psi[, "Est."]
  } else {
    # Simple model
    updated_fit <- fit
    bp <- NULL

    smry <- summary(updated_fit)
    slope_row <- smry$coefficients[2, ]
    intercept_row <- smry$coefficients[1, ]
    betas <- slope_row["Estimate"]
    beta_se <- slope_row["Std. Error"]
    lambdas <- exp(intercept_row["Estimate"])

    params <- list(beta = betas, lambda = lambdas)
    params_se <- list(beta_se = beta_se)
  }

  loglik <- as.numeric(stats::logLik(updated_fit))
  aic <- stats::AIC(updated_fit)
  bic <- stats::BIC(updated_fit)

  fitted_log <- stats::predict(updated_fit)
  resid <- stats::residuals(updated_fit)
  conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)

  result <- list(
    time          = time,
    event         = event,
    cum_events    = cum_events,
    n_obs         = n_obs,
    model         = updated_fit,
    model_type    = "Power Law",
    method        = "LS",
    params        = params,
    params_se     = params_se,
    vcov          = NULL,
    fitted_values = exp(fitted_log),
    lower_bounds  = exp(conf_intervals[, "lwr"]),
    upper_bounds  = exp(conf_intervals[, "upr"]),
    residuals     = resid,
    logLik        = loglik,
    AIC           = aic,
    BIC           = bic,
    breakpoints   = bp,
    conf_level    = conf_level
  )
  class(result) <- "nhpp"
  result
}


#' Print Method for nhpp Objects.
#'
#' Prints a summary of the NHPP model results.
#'
#' @param x An object of class \code{nhpp}.
#' @param ... Additional arguments (not used).
#' @family Repairable Systems Analysis
#' @return Invisibly returns the input object.
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' result <- nhpp(time, event)
#' print(result)
#' @export
print.nhpp <- function(x, ...) {
  if (!inherits(x, "nhpp")) {
    stop("'x' must be an object of class 'nhpp'.")
  }

  cat("Non-Homogeneous Poisson Process (NHPP)\n")
  cat("---------------------------------------\n")
  cat("Model Type:", x$model_type, "\n")
  cat("Estimation Method:", x$method, "\n")
  cat(sprintf("Number of observations: %d\n\n", x$n_obs))

  if (!is.null(x$breakpoints)) {
    cat("Breakpoints (original scale):\n")
    cat(round(exp(x$breakpoints), 4), "\n\n")
  }

  cat("Parameters:\n")
  if (x$model_type == "Log-Linear") {
    cat(sprintf("  a: %.4f (SE = %.4f)\n", x$params$a, x$params_se$a_se))
    cat(sprintf("  b: %.4f (SE = %.4f)\n", x$params$b, x$params_se$b_se))
  } else if (!is.null(x$breakpoints)) {
    # Piecewise
    cat(sprintf("  Betas: %s\n", paste(round(x$params$betas, 4), collapse = ", ")))
    cat(sprintf("  Std. Errors (Betas): %s\n", paste(round(x$params_se$betas_se, 4), collapse = ", ")))
    cat(sprintf("  Lambdas: %s\n", paste(round(x$params$lambdas, 4), collapse = ", ")))
  } else {
    cat(sprintf("  Beta: %.4f (SE = %.4f)\n", x$params$beta, x$params_se$beta_se))
    cat(sprintf("  Lambda: %.4f\n", x$params$lambda))
  }

  cat("\nGoodness of Fit:\n")
  cat(sprintf("  Log-likelihood: %.2f\n", x$logLik))
  cat(sprintf("  AIC: %.2f\n", x$AIC))
  cat(sprintf("  BIC: %.2f\n", x$BIC))

  invisible(x)
}


#' Plot Method for nhpp Objects.
#'
#' Plots observed cumulative events with the fitted NHPP model and optional
#' confidence bounds.
#'
#' @param x An object of class \code{nhpp}.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "topleft").
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Repairable Systems Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' result <- nhpp(time, event)
#' plot(result, main = "Power Law NHPP", xlab = "Time", ylab = "Cumulative Events")
#' @importFrom graphics plot lines abline legend
#' @export
plot.nhpp <- function(x,
                      conf_bounds = TRUE,
                      legend = TRUE,
                      legend_pos = "topleft",
                      ...) {
  if (!inherits(x, "nhpp")) {
    stop("'x' must be an object of class 'nhpp'.")
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

  graphics::plot(x$time, x$cum_events, pch = 16, ...)
  graphics::lines(x$time, x$fitted_values)

  if (conf_bounds) {
    graphics::lines(x$time, x$lower_bounds, lty = 2)
    graphics::lines(x$time, x$upper_bounds, lty = 2)
  }

  if (!is.null(x$breakpoints)) {
    graphics::abline(v = exp(x$breakpoints), lty = 3)
  }

  if (legend) {
    pct <- round(x$conf_level * 100)
    legend_labels <- c("Observed", "Fitted")
    legend_pch <- c(16, NA)
    legend_lty <- c(NA, 1)

    if (conf_bounds) {
      legend_labels <- c(legend_labels, sprintf("Conf. Bounds (%d%%)", pct))
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 2)
    }

    if (!is.null(x$breakpoints)) {
      legend_labels <- c(legend_labels, "Change Points")
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 3)
    }

    graphics::legend(
      legend_pos,
      legend = legend_labels,
      pch = legend_pch,
      lty = legend_lty,
      bty = "n"
    )
  }

  invisible(NULL)
}


#' Overlay Plot for Multiple NHPP Models
#'
#' Plots multiple fitted \code{nhpp} objects on a single set of axes, using
#' distinct colors per model. Observed data points, fitted lines, and optional
#' confidence bounds are drawn for every model. Models may have been fit to
#' different datasets.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} List input is validated to contain only \code{nhpp} objects.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {RE6.0} Model objects have default plot methods.
#' @srrstats {RE6.2} The overlay plot shows fitted values with optional CIs.
#'
#' @param models A named or unnamed list of objects of class \code{nhpp}.
#'   At least one model must be provided. If the list is named, those names
#'   are used as legend labels; otherwise labels default to
#'   \code{"Model 1"}, \code{"Model 2"}, etc.
#' @param conf_bounds Logical; draw confidence bounds for each model
#'   (default: \code{TRUE}).
#' @param legend Logical; draw a legend (default: \code{TRUE}).
#' @param legend_pos Legend position keyword (default: \code{"topleft"}).
#' @param colors Optional character vector of colors, one per model. If
#'   \code{NULL} (default), \code{palette()} colors are cycled.
#' @param ... Additional arguments passed to the initial \code{plot()} call
#'   (e.g., \code{main}, \code{xlab}, \code{ylab}). Not forwarded to subsequent
#'   \code{lines()} or \code{points()} calls.
#' @family Repairable Systems Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' t1 <- c(200, 400, 600, 800, 1000)
#' e1 <- c(3, 5, 4, 7, 6)
#' t2 <- c(300, 600, 900, 1200, 1500)
#' e2 <- c(4, 6, 5, 8, 7)
#' m1 <- nhpp(t1, e1)
#' m2 <- nhpp(t2, e2)
#' overlay_nhpp(list(System_A = m1, System_B = m2),
#'   main = "NHPP Overlay", xlab = "Time",
#'   ylab = "Cumulative Events"
#' )
#' @importFrom graphics plot points lines abline legend
#' @importFrom grDevices palette
#' @export
overlay_nhpp <- function(models,
                         conf_bounds = TRUE,
                         legend = TRUE,
                         legend_pos = "topleft",
                         colors = NULL,
                         ...) {
  # Input validation
  if (!identical(class(models), "list") || length(models) == 0) {
    stop("'models' must be a non-empty list of 'nhpp' objects.")
  }
  not_nhpp <- !vapply(models, inherits, logical(1), what = "nhpp")
  if (any(not_nhpp)) {
    stop("All elements of 'models' must be objects of class 'nhpp'.")
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

  # Compute global axis limits
  all_x <- unlist(lapply(models, `[[`, "time"))
  all_y <- c(
    unlist(lapply(models, `[[`, "cum_events")),
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

  # Base plot using first model's observed data
  graphics::plot(
    models[[1]]$time, models[[1]]$cum_events,
    xlim = xlim, ylim = ylim,
    col = colors[[1]], pch = 16,
    ...
  )

  # Add remaining models' observed points
  if (n_models > 1L) {
    for (i in seq(2L, n_models)) {
      graphics::points(models[[i]]$time, models[[i]]$cum_events,
                       pch = 16, col = colors[[i]])
    }
  }

  # Fitted lines, confidence bounds, and breakpoints for all models
  for (i in seq_len(n_models)) {
    mdl <- models[[i]]
    col <- colors[[i]]

    graphics::lines(mdl$time, mdl$fitted_values, col = col, lty = 1)

    if (conf_bounds) {
      graphics::lines(mdl$time, mdl$lower_bounds, col = col, lty = 2)
      graphics::lines(mdl$time, mdl$upper_bounds, col = col, lty = 2)
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


#' Forecast Cumulative Events from an NHPP Model.
#'
#' Takes a fitted \code{nhpp} object and a vector of future cumulative times,
#' returning predicted cumulative events with confidence bounds.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.13} The function checks for missing data.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#'
#' @param object An object of class \code{nhpp} returned by \code{nhpp()}.
#' @param time A numeric vector of cumulative times at which to forecast.
#'   All values must be finite and > 0.
#' @param conf_level Confidence level (default 0.95).
#' @family Repairable Systems Analysis
#' @return An object of class \code{nhpp_predict} containing:
#' \item{time}{Forecast times.}
#' \item{cum_events}{Predicted cumulative events.}
#' \item{lower_bounds}{Lower confidence bounds.}
#' \item{upper_bounds}{Upper confidence bounds.}
#' \item{conf_level}{Confidence level used.}
#' \item{model_type}{Model type.}
#' \item{nhpp_object}{The original nhpp object.}
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' fit <- nhpp(time, event)
#' fc <- predict_nhpp(fit, time = c(1500, 2000))
#' print(fc)
#' plot(fc, main = "NHPP Forecast", xlab = "Time", ylab = "Cumulative Events")
#' @export
predict_nhpp <- function(object, time, conf_level = 0.95) {
  if (!inherits(object, "nhpp")) {
    stop("'object' must be an object of class 'nhpp'.")
  }
  if (!is.numeric(time) || !is.vector(time)) {
    stop("'time' must be a numeric vector.")
  }
  if (length(time) == 0) {
    stop("'time' cannot be empty.")
  }
  if (any(is.na(time)) || any(is.nan(time))) {
    stop("'time' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop("All values in forecast 'time' must be finite and > 0.")
  }
  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  max_obs_time <- max(object$time)
  if (any(time <= max_obs_time)) {
    warning(
      "Some 'time' values are <= the maximum observed time. ",
      "Hindcasting is allowed but may not be meaningful."
    )
  }

  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)

  if (object$model_type == "Log-Linear") {
    a <- object$params$a
    b <- object$params$b
    if (abs(b) < 1e-12) {
      cum_events <- exp(a) * time
    } else {
      cum_events <- (exp(a) / b) * (exp(b * time) - 1)
    }
    log_fit <- log(pmax(cum_events, 1e-300))

    if (abs(b) < 1e-12) {
      grad_a <- rep(1, length(time))
      grad_b <- time / 2
    } else {
      grad_a <- rep(1, length(time))
      ebt <- exp(b * time)
      grad_b <- (time * ebt) / (ebt - 1) - 1 / b
    }
    grad_mat <- cbind(grad_a, grad_b)
    var_lf <- rowSums((grad_mat %*% object$vcov) * grad_mat)
    hw <- z_val * sqrt(pmax(var_lf, 0))
    lower_bounds <- exp(log_fit - hw)
    upper_bounds <- exp(log_fit + hw)

  } else if (object$method == "MLE") {
    beta <- object$params$beta
    lambda <- object$params$lambda
    cum_events <- lambda * time^beta
    log_fit <- log(cum_events)
    grad_mat <- cbind(log(time), 1 / lambda)
    var_lf <- rowSums((grad_mat %*% object$vcov) * grad_mat)
    hw <- z_val * sqrt(pmax(var_lf, 0))
    lower_bounds <- exp(log_fit - hw)
    upper_bounds <- exp(log_fit + hw)

  } else {
    # LS (including segmented)
    newdata <- data.frame(log_times = log(time))
    pred <- stats::predict(object$model, newdata = newdata,
                           interval = "confidence", level = conf_level)
    cum_events <- exp(pred[, "fit"])
    lower_bounds <- exp(pred[, "lwr"])
    upper_bounds <- exp(pred[, "upr"])
  }

  result <- list(
    time         = time,
    cum_events   = cum_events,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    conf_level   = conf_level,
    model_type   = object$model_type,
    nhpp_object  = object
  )
  class(result) <- "nhpp_predict"
  result
}


#' Print Method for nhpp_predict Objects.
#'
#' Prints a formatted table of forecast cumulative events with confidence bounds.
#'
#' @param x An object of class \code{nhpp_predict}.
#' @param ... Additional arguments (not used).
#' @family Repairable Systems Analysis
#' @return Invisibly returns the input object.
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' fit <- nhpp(time, event)
#' fc <- predict_nhpp(fit, time = c(1500, 2000))
#' print(fc)
#' @export
print.nhpp_predict <- function(x, ...) {
  if (!inherits(x, "nhpp_predict")) {
    stop("'x' must be an object of class 'nhpp_predict'.")
  }

  pct <- round(x$conf_level * 100)
  header <- sprintf("NHPP Forecast (%s)", x$model_type)
  cat(header, "\n")
  cat(paste(rep("-", nchar(header) + 1), collapse = ""), "\n")

  df <- data.frame(
    Time       = x$time,
    Cum.Events = round(x$cum_events, 1),
    Lower      = round(x$lower_bounds, 1),
    Upper      = round(x$upper_bounds, 1),
    check.names = FALSE
  )
  names(df)[3] <- sprintf("Lower (%d%%)", pct)
  names(df)[4] <- sprintf("Upper (%d%%)", pct)

  print(df, row.names = FALSE)

  invisible(x)
}


#' Plot Method for nhpp_predict Objects.
#'
#' Plots observed data, fitted model, and forecast with optional confidence bounds.
#'
#' @param x An object of class \code{nhpp_predict}.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "topleft").
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Repairable Systems Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' time <- c(200, 400, 600, 800, 1000)
#' event <- c(3, 5, 4, 7, 6)
#' fit <- nhpp(time, event)
#' fc <- predict_nhpp(fit, time = c(1500, 2000))
#' plot(fc, main = "NHPP Forecast", xlab = "Time", ylab = "Cumulative Events")
#' @importFrom graphics plot lines abline legend
#' @export
plot.nhpp_predict <- function(x,
                              conf_bounds = TRUE,
                              legend = TRUE,
                              legend_pos = "topleft",
                              ...) {
  if (!inherits(x, "nhpp_predict")) {
    stop("'x' must be an object of class 'nhpp_predict'.")
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

  nhpp_obj <- x$nhpp_object
  obs_times <- nhpp_obj$time
  obs_cum_events <- nhpp_obj$cum_events

  all_y <- c(obs_cum_events, nhpp_obj$fitted_values, x$cum_events)
  if (conf_bounds) {
    all_y <- c(all_y, nhpp_obj$lower_bounds, nhpp_obj$upper_bounds,
               x$lower_bounds, x$upper_bounds)
  }
  xlim <- range(c(obs_times, x$time))
  ylim <- range(all_y)

  graphics::plot(obs_times, obs_cum_events,
                 xlim = xlim, ylim = ylim, pch = 16, ...)
  graphics::lines(obs_times, nhpp_obj$fitted_values)

  if (conf_bounds) {
    graphics::lines(obs_times, nhpp_obj$lower_bounds, lty = 3)
    graphics::lines(obs_times, nhpp_obj$upper_bounds, lty = 3)
  }

  max_obs_time <- max(obs_times)
  graphics::abline(v = max_obs_time, lty = 2, col = "gray")

  graphics::lines(x$time, x$cum_events, lty = 2)

  if (conf_bounds) {
    graphics::lines(x$time, x$lower_bounds, lty = 3)
    graphics::lines(x$time, x$upper_bounds, lty = 3)
  }

  if (legend) {
    pct <- round(x$conf_level * 100)
    legend_labels <- c("Observed", "Fitted", "Forecast")
    legend_pch <- c(16, NA, NA)
    legend_lty <- c(NA, 1, 2)

    if (conf_bounds) {
      legend_labels <- c(legend_labels, sprintf("Conf. Bounds (%d%%)", pct))
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 3)
    }

    graphics::legend(legend_pos,
                     legend = legend_labels,
                     pch = legend_pch,
                     lty = legend_lty,
                     bty = "n",
                     cex = 0.85,
                     y.intersp = 1.3,
                     x.intersp = 0.8)
  }

  invisible(NULL)
}
