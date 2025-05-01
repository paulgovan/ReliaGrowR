

#' Duane Analysis.
#'
#' This function performs a Duane analysis (1962) <doi:10.1109/TA.1964.4319640> on the provided failure data.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param plot Show Duane plot (TRUE) or hide plot (FALSE).
#' @param log Logical indicating whether to use logarithmic scale for the plot (default: TRUE).
#' @param point_col Color for the data points (default: "black").
#' @param line_col Color for the fitted line (default: "black").
#' @param xlab Label for the x-axis (default: "Cumulative Time").
#' @param ylab Label for the y-axis (default: "Cumulative MTBF").
#' @param main Title for the plot (default: "Duane Plot with Cumulative MTBF").
#' @return A list containing the fitted model, AIC, and BIC.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane_plot(times, failures)
#' print(fit)
#' @importFrom stats lm coef
#' @importFrom graphics legend lines
#' @export

duane_plot <- function(times, failures, plot = TRUE, log = TRUE,
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

  # Fit a linear model to the log-transformed data for Duane analysis
  log_cum_time <- log(cum_time)
  log_cum_mtbf <- log(cum_mtbf)
  fit <- stats::lm(log_cum_mtbf ~ log_cum_time)

  # Extract goodness of fit metrics
  aic <- stats::AIC(fit)
  bic <- stats::BIC(fit)

  # Calculate fitted values on the original scale
  fitted_values <- exp(predict(fit))

  # Plot
  if (plot) {
    if (log) {
      plot(cum_time, cum_mtbf, log = "xy", pch = 16, col = point_col, xlab = xlab,
           ylab = ylab, main = main)
      lines(cum_time, fitted_values, col = line_col, lty = 1)
    } else {
      plot(cum_time, cum_mtbf, pch = 16, col = point_col, xlab = xlab, ylab = ylab,
           main = main)
      lines(cum_time, fitted_values, col = line_col, lty = 1)
    }

    legend("bottomright", legend = c("Observed", "Fitted Line"),
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

  class(result) <- "duane"  # Assign custom S3 class

  return(result)
}


#' Print method for duane objects.
#'
#' This function prints a summary of the Duane analysis result.
#'
#' @param x An object of class "duane" returned by the duane_plot function.
#' @param ... Additional arguments (not used).
#' @export
print.duane <- function(x, ...) {
  cat("Duane Analysis Result\n")
  cat("----------------------\n")
  cat("Linear model (log-log scale): log(MTBF) ~ log(Time)\n")
  cat("\nCoefficients:\n")
  print(coef(x$model))
  cat(sprintf("\nAIC: %.2f, BIC: %.2f\n", x$AIC, x$BIC))
  invisible(x)
}
