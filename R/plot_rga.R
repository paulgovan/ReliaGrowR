#' Plotting Function for Reliability Growth Analysis.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param result A list of results for a Reliability Growth Analysis.
#' @param conf_level the desired confidence level, which defaults to 95%.
#' @return The function does not return a value.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' plot_rga(times, failures, result)
#' @importFrom graphics legend lines
#' @export

plot_rga <- function(times, failures, result, conf_level = 0.95) {

  # Extract the cumulative failures and the model
  cum_failures <- result$cum_failures
  model <- result$model

  # Generate predicted values using the model
  predicted <- exp(predict(model))

  # Calculate the confidence bounds for the predicted values
  lower_predicted <- exp(result$lower_lambda) * times^result$lower_beta
  upper_predicted <- exp(result$upper_lambda) * times^result$upper_beta

  # Set up the plot
  plot(times, cum_failures, pch = 16, col = "blue",
       xlab = "Time", ylab = "Cumulative Failures",
       main = paste("Crow-AMSAA Model with", conf_level * 100, "% Confidence Bounds"))

  # Add the predicted line
  lines(times, predicted, col = "red", lty = 1)

  # Add the confidence bounds
  lines(times, lower_predicted, col = "red", lty = 2)
  lines(times, upper_predicted, col = "red", lty = 2)

  # Add a legend
  legend("bottomright", legend = c("Observed", "Predicted", "Confidence Bounds"),
         col = c("blue", "red", "red"), pch = c(16, NA, NA), lty = c(NA, 1, 2))
}
