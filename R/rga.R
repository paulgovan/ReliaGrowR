#' Reliability Growth Analysis.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise Weibull NHPP` with change point detection.
#' @param breakpoints An optional vector of breakpoints for the `Piecewise Weibull NHPP` model.
#' @param conf_level The desired confidence level, which defaults to 95%.
#' @return The function returns an object of class `rga` that contains the results for the model.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#' @importFrom stats lm predict
#' @importFrom segmented segmented
#' @export

rga <- function(times, failures, model_type = "Crow-AMSAA", breakpoints = NULL, conf_level = 0.95) {

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
  valid_models <- c("Crow-AMSAA", "Piecewise Weibull NHPP")
  if (!(model_type %in% valid_models)) {
    stop(paste("Model_type must be one of", paste(valid_models, collapse = ", "), "."))
  }

  # Check if breakpoints are valid if provided
  if (!is.null(breakpoints)) {
    if (!is.numeric(breakpoints) || any(breakpoints <= 0)) {
      stop("Breakpoints must be a numeric vector with positive values.")
    }
    if (model_type != "Piecewise Weibull NHPP") {
      stop("Breakpoints can only be used with the 'Piecewise Weibull NHPP' model.")
    }
  }

  # Convert to cumulative failure times
  cum_failures <- cumsum(failures)

  # Log-transform the data
  log_times <- log(times)
  log_cum_failures <- log(cum_failures)

  # Fit a linear model to the log-transformed data
  fit <- stats::lm(log_cum_failures ~ log_times)

  if (model_type == "Piecewise Weibull NHPP") {
    if (is.null(breakpoints)) {
      # Apply the segmented package to detect change points
      updated_fit <- segmented::segmented(fit, seg.Z = ~log_times)

      # Extract the breakpoints (change points)
      breakpoints <- updated_fit$psi[, "Est."]
    } else {
      # Apply the user-supplied breakpoints
      segmented_fit <- segmented::segmented(fit, seg.Z = ~log_times, fixed.psi = log(breakpoints))

      # Update the model fit with the user-supplied breakpoints
      updated_fit <- segmented_fit
    }

    # Calculate the Weibull parameters for each segment
    slopes <- updated_fit$coefficients[2] / log_times
    intercepts <- updated_fit$coefficients[1]
    shape_parameters <- 1 / slopes
    scale_parameters <- exp(intercepts)

    # Calculate Beta (slope) and Lambda (intercept) for each segment
    betas <- updated_fit$coefficients[2]
    lambdas <- exp(updated_fit$coefficients[1])

  } else {
    updated_fit <- fit
    breakpoints <- NULL

    # Calculate Weibull parameters for the Crow-AMSAA model
    slope <- updated_fit$coefficients[2]
    intercept <- updated_fit$coefficients[1]
    shape_parameters <- 1 / slope
    scale_parameters <- exp(intercept)

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

  # Return the segmented model, breakpoints, coefficients, confidence bounds, Weibull parameters, Beta, and Lambda
  result <- (
    list(
      model = updated_fit,
      AIC = aic,
      BIC = bic,
      breakpoints = breakpoints,
      fitted_values = exp(fitted_values),
      lower_bounds = lower_bounds,
      upper_bounds = upper_bounds,
      shape_parameters = shape_parameters,
      scale_parameters = scale_parameters,
      betas = betas,
      lambdas = lambdas
    )
  )

  class(result) <- "rga"  # Assign the custom S3 class

  return(result)
}

# Custom print method for rga objects
print.rga <- function(x, ...) {
  cat("Reliability Growth Analysis (RGA) Results:\n")
  cat("----------------------------------------------------\n")
  cat("Model Type:        ", ifelse(is.null(x$breakpoints), "Crow-AMSAA", "Piecewise Weibull NHPP"), "\n")
  if (!is.null(x$breakpoints)) {
    cat("Breakpoints:       ", paste(round(x$breakpoints, 2), collapse = ", "), "\n")
  }
  cat("Shape Parameters:  ", paste(round(x$shape_parameters, 2), collapse = ", "), "\n")
  cat("Scale Parameters:  ", paste(round(x$scale_parameters, 2), collapse = ", "), "\n")
  cat("Betas (slopes):    ", paste(round(x$betas, 2), collapse = ", "), "\n")
  cat("Lambdas:           ", paste(round(x$lambdas, 2), collapse = ", "), "\n")
  cat("----------------------------------------------------\n")
  invisible(x)  # Ensure the object is returned invisibly
}
