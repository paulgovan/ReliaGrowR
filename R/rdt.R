#' Reliability Demonstration Test Design
#'
#' @param sample_size A numeric value indicating the number of samples to be tested. If NULL, the function will solve for sample size given a test time.
#' @param test_time A numeric value indicating the duration of the test (e.g., in hours). If NULL, the function will solve for test time given a sample size.
#' @param target A numeric value between 0 and 1 representing the desired target reliability.
#' @param conf_level A numeric value between 0 and 1 indicating the confidence level (e.g., 0.90 for 90% confidence).
#' @param failure_rate A numeric value for the failure rate, required only for the exponential distribution.
#' @param dist_type A character string specifying the distribution type. Options are "exponential", "Weibull", or "lognormal". Default is "exponential".
#' @param shape A numeric value representing the shape parameter. Required for Weibull (`beta`) and Lognormal (`sdlog`) distributions.
#' @param scale A numeric value representing the scale parameter. Required for Weibull (`eta`) and Lognormal (`meanlog`) distributions.
#' @param plot A logical value indicating whether to plot the trade-off between sample size and test time. Default is TRUE.
#'
#' @return A list containing the calculated test time or sample size, target reliability, confidence level, failure rate, and distribution parameters.
#'
#' @examples
#' # Example for Exponential Distribution
#' rdt(
#'   sample_size = 100,
#'   target = 0.95,
#'   conf_level = 0.90,
#'   failure_rate = 0.01,
#'   dist_type = "exponential"
#' )
#'
#' # Example for Weibull Distribution with Visualization
#' rdt(
#'   sample_size = 50,
#'   target = 0.95,
#'   conf_level = 0.90,
#'   dist_type = "Weibull",
#'   shape = 1.5,
#'   scale = 200,
#'   plot = TRUE
#' )
#' @importFrom graphics grid
#' @importFrom stats qlnorm qnorm
#' @export
rdt <- function(sample_size = NULL, test_time = NULL,
                target, conf_level, failure_rate = NULL,
                dist_type = "exponential", shape = NULL, scale = NULL, plot = TRUE) {

  # Ensure inputs are valid
  if (target <= 0 || target >= 1) stop("Target reliability should be between 0 and 1.")
  if (conf_level <= 0 || conf_level >= 1) stop("Confidence level should be between 0 and 1.")

  # z-value for the confidence level
  z_value <- qnorm(conf_level)

  if (dist_type == "exponential") {
    if (is.null(failure_rate)) stop("Failure rate is required for the exponential distribution.")

    # For exponential distribution: R(t) = exp(-lambda * t)
    lambda <- -log(target)
    if (is.null(test_time)) {
      test_time <- (z_value^2) / (2 * sample_size * failure_rate * lambda)
    } else {
      sample_size <- (z_value^2) / (2 * test_time * failure_rate * lambda)
    }

  } else if (dist_type == "Weibull") {
    if (is.null(shape) || is.null(scale)) stop("Both shape and scale parameters are required for the Weibull distribution.")

    lambda <- -log(target)
    if (is.null(test_time)) {
      test_time <- scale * (lambda / (z_value^2 / (2 * sample_size)))^(1 / shape)
    } else {
      sample_size <- (z_value^2) / (2 * (test_time / scale)^shape)
    }

  } else if (dist_type == "lognormal") {
    if (is.null(shape) || is.null(scale)) stop("Both shape and scale parameters are required for the lognormal distribution.")

    lambda <- -log(target)
    if (is.null(test_time)) {
      test_time <- qlnorm(1 - lambda, meanlog = scale, sdlog = shape)
    } else {
      sample_size <- (z_value^2) / (2 * log(test_time / scale) / shape)
    }

  } else {
    stop("Unsupported distribution type. Choose 'exponential', 'Weibull', or 'lognormal'.")
  }

  # Round up sample size
  sample_size <- ceiling(sample_size)

  # Plot the trade-off between sample size and test time
  if (plot) {
    # Create a sequence of sample sizes and calculate corresponding test times
    sample_sizes <- seq(10, sample_size + 50, by = 5)

    if (dist_type == "exponential") {
      test_times <- (z_value^2) / (2 * sample_sizes * failure_rate * lambda)
    } else if (dist_type == "Weibull") {
      test_times <- scale * (lambda / (z_value^2 / (2 * sample_sizes)))^(1 / shape)
    } else if (dist_type == "lognormal") {
      test_times <- qlnorm(1 - lambda, meanlog = scale, sdlog = shape)
      # Adjust length to match sample_sizes in case of vector mismatch
      test_times <- rep(test_times, length(sample_sizes))
    }

    # Plot function
    plot(sample_sizes, test_times, type = "l", col = "blue", lwd = 2,
         xlab = "Sample Size", ylab = "Test Time",
         main = "Sample Size vs Test Time",
         sub = paste("Distribution:", dist_type))
    grid()
  }

  # Return results
  result <- list(
    Sample_Size = sample_size,
    Test_Time = test_time,
    target = target,
    conf_level = conf_level,
    Failure_Rate = failure_rate,
    Distribution_Type = dist_type,
    shape = shape,
    scale = scale
  )

  return(result)
}
