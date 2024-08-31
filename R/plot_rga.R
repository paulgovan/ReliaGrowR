#' Plotting Function for Reliability Growth Analysis.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param result A list of results for a Reliability Growth Analysis.
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(times, failures, result)
#' @importFrom graphics legend lines
#' @export

plot_rga <- function(times, failures, result) {

  # Check if the inputs are valid
  if (length(times) != length(failures)) {
    stop("Error: The length of 'times' and 'failures' must be equal.")
  }

  if (!is.numeric(times) || !is.numeric(failures)) {
    stop("Error: Both 'times' and 'failures' must be numeric vectors.")
  }

  if (any(times <= 0)) {
    stop("Error: All values in 'times' must be greater than 0.")
  }

  if (!is.list(result) || !all(c("fitted_values", "lower_bounds", "upper_bounds") %in% names(result))) {
    stop("Error: 'result' must be a list containing 'fitted_values', 'lower_bounds', and 'upper_bounds'.")
  }

  # Convert to cumulative failure times
  cum_failures <- cumsum(failures)

  # Set up the plot
  plot(times, cum_failures, pch = 16, col = "blue",
       xlab = "Time", ylab = "Cumulative Failures",
       main = "Reliability Growth Analysis")

  # Add the fitted line(s)
  lines(times, result$fitted_values, col = "red", lty = 1)

  # Add the confidence bounds
  lines(times, result$lower_bounds, col = "red", lty = 2)
  lines(times, result$upper_bounds, col = "red", lty = 2)

  # Add vertical lines at the change points
  if (!is.null(result$breakpoints)) {
    abline(v = exp(result$breakpoints), col = "green", lty = 3)
  }

  # Add a legend
  legend("bottomright", legend = c("Observed", "Fitted Line", "Confidence Bounds", "Change Points"),
         col = c("blue", "red", "red", "green"), pch = c(16, NA, NA, NA), lty = c(NA, 1, 2, 3))
}
