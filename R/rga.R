

#' Reliability Growth Analysis.
#'
#' This function performs reliability growth analysis using the Crow-AMSAA model by
#' Crow (1975) <https://apps.dtic.mil/sti/citations/ADA020296> or piecewise
#' NHPP model by Guo et al. (2010) <doi:10.1109/RAMS.2010.5448029>.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise NHPP` with change point detection.
#' @param breaks An optional vector of breakpoints for the `Piecewise NHPP` model.
#' @param conf_level The desired confidence level, which defaults to 95%.
#' @return The function returns an object of class `rga` that contains the results for the model.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#' @importFrom stats lm predict
#' @importFrom segmented segmented slope intercept seg.control
#' @export
rga <- function(times, failures, model_type = "Crow-AMSAA", breaks = NULL, conf_level = 0.95) {

  # Check if the inputs are valid
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }

  if (any(times <= 0)) {
    stop("All values in 'times' must be greater than 0.")
  }

  if (any(failures <= 0)) {
    stop("All values in 'failures' must be greater than 0.")
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("Conf_level must be a numeric value between 0 and 1 (exclusive).")
  }

  # Check if the model_type is valid
  valid_models <- c("Crow-AMSAA", "Piecewise NHPP")
  if (!(model_type %in% valid_models)) {
    stop(paste("Model_type must be one of", paste(valid_models, collapse = ", "), "."))
  }

  # Check if breaks are valid if provided
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || any(breaks <= 0)) {
      stop("Breakpoints must be a numeric vector with positive values.")
    }
    if (model_type != "Piecewise NHPP") {
      stop("Breakpoints can only be used with the 'Piecewise NHPP' model.")
    }
  }

  # Convert to cumulative failure times and cumulative operating time
  cum_failures <- cumsum(failures)
  cum_time <- cumsum(times)

  # Log-transform the data
  log_times <- log(cum_time)
  log_cum_failures <- log(cum_failures)

  # Fit a linear model to the log-transformed data
  fit <- stats::lm(log_cum_failures ~ log_times)

    if (model_type == "Piecewise NHPP") {
      if (is.null(breaks)) {
        # Apply the segmented package to detect change points
        updated_fit <- segmented::segmented(fit, seg.Z = ~log_times)
        # Extract the breakpoints (change points)
        breakpoints <- updated_fit$psi[, "Est."]
      } else {
        # Apply the user-supplied breakpoints
        breakpoints <- log(breaks)
        updated_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = breakpoints)
      }

      # Extract the slope for each segment and convert to beta (shape parameter)
      slopes <- segmented::slope(updated_fit)
      intercepts <- segmented::intercept(updated_fit)

      # Calculate Beta (slope) and Lambda (intercept) for each segment
      betas <- slopes
      lambdas <- exp(intercepts$log_times)

    } else {
      updated_fit <- fit
      breakpoints <- NULL

      # Calculate parameters for the Crow-AMSAA model
      summary <- summary(updated_fit)
      slope <- summary$coefficients[2,]
      intercept <- summary$coefficients[1,]

      # Calculate Beta and Lambda for the Crow-AMSAA model
      betas <- slope
      lambdas <- exp(intercept)
    }

    # Extract goodness of fit metrics
    aic <- stats::AIC(updated_fit)
    bic <- stats::BIC(updated_fit)

    # Generate the fitted values and confidence intervals
    fitted_values <- stats::predict(updated_fit)
    conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)

    lower_bounds <- exp(conf_intervals[, "lwr"])
    upper_bounds <- exp(conf_intervals[, "upr"])

    # Return the segmented model, breakpoints, coefficients, confidence bounds, parameters, Beta, and Lambda
    result <- (
      list(
        model = updated_fit,
        AIC = aic,
        BIC = bic,
        breakpoints = breakpoints,
        fitted_values = exp(fitted_values),
        lower_bounds = lower_bounds,
        upper_bounds = upper_bounds,
        betas = betas,
        lambdas = lambdas
      )
    )

    class(result) <- "rga"  # Assign the custom S3 class

    return(result)
  }

#' Print method for rga objects.
#'
#' This function prints a summary of the RGA analysis result.
#' @param x An object of class `rga`.
#' @param ... Additional arguments (not used).
#' @export
print.rga <- function(x, ...) {
  cat("Reliability Growth Analysis (RGA)\n")
  cat("---------------------------------\n")

  # Determine model type
  model_type <- if (is.null(x$breakpoints)) "Crow-AMSAA" else "Piecewise NHPP"
  cat("Model Type:", model_type, "\n\n")

  # Print breakpoints if available
  if (!is.null(x$breakpoints)) {
    cat("Breakpoints (original scale):\n")
    cat(round(exp(x$breakpoints), 4), "\n")
    cat("\n")
  }

  # Print parameters
  cat("Parameters (per segment):\n")
  if (model_type == "Piecewise NHPP") {
    betas <- round(x$betas$log_times[, "Est."], 4)
    cat(sprintf("  Betas: %s\n", paste(betas, collapse = ", ")))
  } else {
    cat(sprintf("  Beta: %.4f\n", x$betas[1]))
  }

  if (model_type == "Piecewise NHPP") {
    lambdas <- round(x$lambdas[, "Est."], 4)
    cat(sprintf("  Lambdas: %s\n", paste(lambdas, collapse = ", ")))
  } else {
    cat(sprintf("  Lambda: %.4f\n", x$lambdas[1]))
  }

  # Goodness of fit
  cat("\nGoodness of Fit:\n")
  cat(sprintf("  AIC: %.2f\n", x$AIC))
  cat(sprintf("  BIC: %.2f\n", x$BIC))

  invisible(x)
}


