#' Plotting Function for Reliability Growth Analysis
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param result A list of results from the RGA model.
#' @param point_col Color for the data points (default: "blue").
#' @param line_col Color for the fitted line (default: "red").
#' @param xlab Label for the x-axis (default: "Cumulative Time").
#' @param ylab Label for the y-axis (default: "Cumulative Failures").
#' @param main Title for the plot (default: "Reliability Growth Analysis").
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(times, failures, result)
#' @importFrom graphics lines abline legend
#' @export

plot_rga <- function(times, failures, result,
                     point_col = "blue", line_col = "red",
                     xlab = "Cumulative Time", ylab = "Cumulative Failures",
                     main = "Reliability Growth Analysis") {

  # Check if the inputs are valid
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }

  if (any(times <= 0)) {
    stop("All values in 'times' must be greater than 0.")
  }

  # Convert to cumulative failure times
  cum_failures <- cumsum(failures)

  # Set up the plot
  plot(times, cum_failures, pch = 16, col = point_col,
       xlab = xlab, ylab = ylab,
       main = main)

  # Add the fitted line(s)
  graphics::lines(times, result$fitted_values, col = line_col, lty = 1)

  # Add the confidence bounds
  graphics::lines(times, result$lower_bounds, col = line_col, lty = 2)
  graphics::lines(times, result$upper_bounds, col = line_col, lty = 2)

  # Add vertical lines at the change points
  if (!is.null(result$breakpoints)) {
    graphics::abline(v = exp(result$breakpoints), col = "green", lty = 3)
  }

  # Add a legend
  graphics::legend("bottomright", legend = c("Observed", "Fitted Line", "Confidence Bounds", "Change Points"),
         col = c(point_col, line_col, line_col, "green"), pch = c(16, NA, NA, NA), lty = c(NA, 1, 2, 3))
}
