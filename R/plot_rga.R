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
#' @param legend_pos Position of the legend (default: "bottomright").
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(result)
#' @importFrom graphics lines abline legend plot
#' @export
plot_rga <- function(rga_obj,
                     point_col = "black", line_col = "black",
                     xlab = "Cumulative Time", ylab = "Cumulative Failures",
                     main = "Reliability Growth Analysis",
                     conf_bounds = TRUE,
                     legend = TRUE,
                     log = FALSE,
                     legend_pos = "bottomright") {

  # Basic input checks
  if (!inherits(rga_obj, "rga")) stop("Input must be an object of class 'rga'.")
  if (!is.logical(conf_bounds) || !is.logical(legend) || !is.logical(log)) {
    stop("Arguments 'conf_bounds', 'legend', and 'log' must be logical.")
  }

  # Extract original log-log model data
  if (!all(c("log_times", "log_cum_failures") %in% names(rga_obj$model$model))) {
    stop("The 'rga_obj' appears malformed or missing model data.")
  }

  times <- exp(rga_obj$model$model$log_times)
  cum_failures <- exp(rga_obj$model$model$log_cum_failures)

  # Set up base plot
  plot_args <- list(
    x = times,
    y = cum_failures,
    pch = 16,
    col = point_col,
    xlab = xlab,
    ylab = ylab,
    main = main
  )

  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # Plot fitted values
  graphics::lines(times, rga_obj$fitted_values, col = line_col, lty = 1)

  # Confidence bounds
  if (conf_bounds) {
    graphics::lines(times, rga_obj$lower_bounds, col = line_col, lty = 2)
    graphics::lines(times, rga_obj$upper_bounds, col = line_col, lty = 2)
  }

  # Change points (breakpoints)
  if (!is.null(rga_obj$breakpoints)) {
    graphics::abline(v = exp(rga_obj$breakpoints), col = "black", lty = 3)
  }

  # Legend
  if (legend) {
    legend_items <- list(
      labels = c("Observed", "Fitted Line"),
      cols = c(point_col, line_col),
      pch = c(16, NA),
      lty = c(NA, 1)
    )

    if (conf_bounds) {
      legend_items$labels <- c(legend_items$labels, "Confidence Bounds")
      legend_items$cols <- c(legend_items$cols, line_col)
      legend_items$pch <- c(legend_items$pch, NA)
      legend_items$lty <- c(legend_items$lty, 2)
    }

    if (!is.null(rga_obj$breakpoints)) {
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
