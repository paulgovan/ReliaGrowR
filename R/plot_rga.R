#' Plotting Function for Reliability Growth Analysis
#'
#' @param rga_obj An object of class `rga`, which contains the results from the RGA model.
#' @param point_col Color for the data points (default: "black").
#' @param line_col Color for the fitted line (default: "black").
#' @param xlab Label for the x-axis (default: "Cumulative Time").
#' @param ylab Label for the y-axis (default: "Cumulative Failures").
#' @param main Title for the plot (default: "Reliability Growth Analysis").
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(result)
#' @importFrom graphics lines abline legend
#' @export
plot_rga <- function(rga_obj,
                     point_col = "black", line_col = "black",
                     xlab = "Cumulative Time", ylab = "Cumulative Failures",
                     main = "Reliability Growth Analysis") {

  # Extract times and cumulative failures from the rga object
  times <- exp(rga_obj$model$model$log_times)
  cum_failures <- exp(rga_obj$model$model$log_cum_failures)

  # Set up the plot
  plot(times, cum_failures, pch = 16, col = point_col,
       xlab = xlab, ylab = ylab,
       main = main)

  # Add the fitted line(s)
  graphics::lines(times, rga_obj$fitted_values, col = line_col, lty = 1)

  # Add the confidence bounds
  graphics::lines(times, rga_obj$lower_bounds, col = line_col, lty = 2)
  graphics::lines(times, rga_obj$upper_bounds, col = line_col, lty = 2)

  # Add vertical lines at the change points
  if (!is.null(rga_obj$breakpoints)) {
    graphics::abline(v = exp(rga_obj$breakpoints), col = "black", lty = 3)
  }

  # Add a legend
  graphics::legend("bottomright", legend = c("Observed", "Fitted Line", "Confidence Bounds", "Change Points"),
                   col = c(point_col, line_col, line_col, "black"), pch = c(16, NA, NA, NA), lty = c(NA, 1, 2, 3))
}
