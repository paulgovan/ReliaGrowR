#' Reliability Growth Analysis.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param model_type The model type. Either `Crow-AMSAA` (default) or `Piecewise Weibull NHPP` with change point detection.
#' @param conf_level the desired confidence level, which defaults to 95%.
#' @return The function returns a list of the results for the model.
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
    stop("Error: All values in 'failures' must be greater than 0.")
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("Error: conf_level must be a numeric value between 0 and 1 (exclusive).")
  }

  # Check if the model_type is valid
  valid_models <- c("Crow-AMSAA", "Piecewise Weibull NHPP")
  if (!(model_type %in% valid_models)) {
    stop(paste("Error: model_type must be one of", paste(valid_models, collapse = ", "), "."))
  }

  # Check if breakpoints are valid if provided
  if (!is.null(breakpoints)) {
    if (!is.numeric(breakpoints) || any(breakpoints <= 0)) {
      stop("Error: breakpoints must be a numeric vector with positive values.")
    }
    if (model_type != "Piecewise Weibull NHPP") {
      stop("Error: breakpoints can only be used with the 'Piecewise Weibull NHPP' model.")
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
      segmented_fit <- segmented::segmented(fit, seg.Z = ~log_times, psi = log(breakpoints))

      # Update the model fit with the user-supplied breakpoints
      updated_fit <- segmented_fit
    }
  } else {
    updated_fit <- fit
    breakpoints <- NULL
  }

  # Generate the fitted values and confidence intervals
  fitted_values <- stats::predict(updated_fit)
  conf_intervals <- stats::predict(updated_fit, interval = "confidence", level = conf_level)

  lower_bounds <- exp(conf_intervals[, "lwr"])
  upper_bounds <- exp(conf_intervals[, "upr"])

  # Return the segmented model, breakpoints, coefficients, and confidence bounds
  return(list(
    model = updated_fit,
    breakpoints = breakpoints,
    fitted_values = exp(fitted_values),
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds
  ))
}
