#' Reliability Growth Analysis using the Crow-AMSAA model.
#'
#' @param times A vector of cumulative times at which failures occurred.
#' @param failures A vector of the number of failures at each corresponding time in times.
#' @param conf_level the desired confidence level, which defaults to 95%.
#' @return The function returns a list of the results for the model.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' result <- rga(times, failures)
#' print(result)
#' @importFrom stats coef lm predict qt
#' @export

rga <- function(times, failures, conf_level = 0.95) {

  # Check if the inputs are valid
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }

  if (any(times <= 0)) {
    stop("All values in 'times' must be greater than 0.")
  }

  # Convert to cumulative failure times
  cum_failures <- cumsum(failures)

  # Log-transform the data
  log_times <- log(times)
  log_cum_failures <- log(cum_failures)

  # Fit a linear model to the log-transformed data
  fit <- lm(log_cum_failures ~ log_times)

  # Extract the model parameters
  beta <- coef(fit)[2]
  lambda <- exp(coef(fit)[1])

  # Calculate the standard error of the slope and intercept
  se <- summary(fit)$coefficients[, 2]

  # Calculate the confidence bounds for lambda and beta
  alpha <- 1 - conf_level
  t_value <- qt(1 - alpha / 2, df = length(times) - 2)

  lower_lambda <- exp(coef(fit)[1] - t_value * se[1])
  upper_lambda <- exp(coef(fit)[1] + t_value * se[1])

  lower_beta <- coef(fit)[2] - t_value * se[2]
  upper_beta <- coef(fit)[2] + t_value * se[2]

  # Return the parameters, the model fit, and confidence bounds
  return(list(
    lambda = lambda,
    beta = beta,
    model = fit,
    cum_failures = cum_failures,
    lower_lambda = lower_lambda,
    upper_lambda = upper_lambda,
    lower_beta = lower_beta,
    upper_beta = upper_beta
  ))
}
