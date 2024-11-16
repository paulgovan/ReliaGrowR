#' Plotting Function for Duane Analysis.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param plot Show Duane Plot (TRUE) or hide plot (FALSE).
#' @param point_col Color for the data points (default: "black").
#' @param line_col Color for the fitted line (default: "black").
#' @param xlab Label for the x-axis (default: "Cumulative Time").
#' @param ylab Label for the y-axis (default: "Cumulative MTBF").
#' @param main Title for the plot (default: "Duane Plot with Cumulative MTBF").
#' @return The function returns a list of the fitted linear model, Cumulative Time, Cumulative MTBF.
#' @examples
#' library(ReliaGrowR)
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane_plot(times, failures)
#' summary(fit)
#' @importFrom stats lm
#' @importFrom graphics legend lines
#' @export

duane_plot <- function(times, failures, plot = TRUE,
                       point_col = "black", line_col = "black",
                       xlab = "Cumulative Time", ylab = "Cumulative MTBF",
                       main = "Duane Plot with Cumulative MTBF") {

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

  # Convert to cumulative failure times and cumulative operating time
  cum_failures <- cumsum(failures)
  cum_time <- cumsum(times)

  # Calculate cumulative MTBF (Mean Time Between Failures)
  cum_mtbf <- cum_time / cum_failures

  # Set up the plot with log-log scale if plot is TRUE
  if (plot) {
    plot(cum_time, cum_mtbf, log = "xy", pch = 16, col = point_col,
         xlab = xlab, ylab = ylab, main = main)
  }

  # Fit a linear model to the log-transformed data for the Duane plot
  log_cum_time <- log(cum_time)
  log_cum_mtbf <- log(cum_mtbf)
  fit <- stats::lm(log_cum_mtbf ~ log_cum_time)

  # Extract goodness of fit metrics
  aic <- stats::AIC(fit)
  bic <- stats::BIC(fit)

  # Add the fitted line if plot is TRUE
  fitted_values <- exp(predict(fit))
  if (plot) {
    graphics::lines(cum_time, fitted_values, col = line_col, lty = 1)
  }

  # Add a legend if plot is TRUE
  if (plot) {
    graphics::legend("bottomright", legend = c("Observed", "Fitted Line"),
           col = c(point_col, line_col), pch = c(16, NA), lty = c(NA, 1))
  }

  result <- list(
    model = fit,
    AIC = aic,
    BIC = bic,
    Cumulative_Time = cum_time,
    Cumulative_MTBF = cum_mtbf,
    Fitted_Values = fitted_values
  )

  class(result) <- "duane"  # Assign the custom S3 class

  return(result)
}
