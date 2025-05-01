

#' Plot Reliability Growth Analysis Results
#'
#' This function generates a plot for the results of a Reliability Growth Analysis (RGA).
#'
#' @param rga_obj An object of class `rga`, which contains the results from the RGA model.
#' @param point_col Color for the data points (default: "black").
#' @param line_col Color for the fitted line (default: "black").
#' @param xlab Label for the x-axis (default: "Cumulative Time").
#' @param ylab Label for the y-axis (default: "Cumulative Failures").
#' @param main Title for the plot (default: "Reliability Growth Analysis").
#' @param conf_bounds Logical indicating whether to include confidence bounds (default: TRUE).
#' @param legend Logical indicating whether to show the legend (default: TRUE).
#' @param log Logical indicating whether to use a log-log scale (default: FALSE).
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(result)
#' @importFrom graphics lines abline legend
#' @export
#' @export
plot_rga <- function(rga_obj,
                     point_col = "black", line_col = "black",
                     xlab = "Cumulative Time", ylab = "Cumulative Failures",
                     main = "Reliability Growth Analysis",
                     conf_bounds = TRUE,
                     legend = TRUE,
                     log = FALSE) {

  # Extract times and cumulative failures
  times <- exp(rga_obj$model$model$log_times)
  cum_failures <- exp(rga_obj$model$model$log_cum_failures)

  # Set up plot scale
  plot_args <- list(x = times, y = cum_failures, pch = 16, col = point_col,
                    xlab = xlab, ylab = ylab, main = main)

  if (log) {
    plot_args$log <- "xy"
  }

  do.call(plot, plot_args)

  # Add the fitted line(s)
  graphics::lines(times, rga_obj$fitted_values, col = line_col, lty = 1)

  # Add confidence bounds if requested
  if (conf_bounds) {
    graphics::lines(times, rga_obj$lower_bounds, col = line_col, lty = 2)
    graphics::lines(times, rga_obj$upper_bounds, col = line_col, lty = 2)
  }

  # Add vertical lines for change points if present
  if (!is.null(rga_obj$breakpoints)) {
    graphics::abline(v = exp(rga_obj$breakpoints), col = "black", lty = 3)
  }

  # Add legend if requested
  if (legend) {
    legend_labels <- c("Observed", "Fitted Line")
    legend_cols <- c(point_col, line_col)
    legend_pch <- c(16, NA)
    legend_lty <- c(NA, 1)

    if (conf_bounds) {
      legend_labels <- c(legend_labels, "Confidence Bounds")
      legend_cols <- c(legend_cols, line_col)
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 2)
    }

    if (!is.null(rga_obj$breakpoints)) {
      legend_labels <- c(legend_labels, "Change Points")
      legend_cols <- c(legend_cols, "black")
      legend_pch <- c(legend_pch, NA)
      legend_lty <- c(legend_lty, 3)
    }

    graphics::legend("bottomright", legend = legend_labels,
                     col = legend_cols, pch = legend_pch, lty = legend_lty)
  }
}
