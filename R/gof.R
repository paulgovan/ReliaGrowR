
#' Q-Q Plot for RGA Objects
#'
#' This function creates a Q-Q plot for a fitted Reliability Growth Analysis (RGA) model
#' Currently only supports the Crow-AMSAA model.
#'
#' @param x An object of class \code{rga}.
#' @param main Title of the plot.
#' @param ... Additional arguments passed to \code{stats::qqplot()}.
#' @examples
#' times <- c(5, 10, 15, 20, 25)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' qqplot.rga(fit)
#' @return A Q-Q plot comparing empirical and theoretical quantiles.
#' @family goodness-of-fit
#' @importFrom stats lsfit
#' @export
qqplot.rga <- function(x,
                       main = "Q-Q Plot",
                       ...) {
  if (!inherits(x, "rga")) stop("Input must be of class 'rga'.")

  # Extract parameters (Crow-AMSAA case)
  beta <- if (is.list(x$betas)) x$betas$log_times[1, "Est."] else x$betas[1]
  lambda <- if (is.matrix(x$lambdas)) x$lambdas[1, "Est."] else x$lambdas[1]

  # Observed cumulative times
  times <- sort(
    exp(x$model$model$log_times)
    )
  n <- length(times)

  # Empirical probs
  emp_probs <- (1:n) / (n + 1)

  # Theoretical quantiles
  theo_quants <- (-log(1 - emp_probs) / lambda)^(1 / beta)

  stats::qqplot(theo_quants, times,
                main = main,
                xlab = "Theoretical Quantiles",
                ylab = "Observed Quantiles",
                ...)
  abline(lsfit(theo_quants, times))
}

#' P-P Plot for RGA Objects
#'
#' This function creates a P-P plot for a fitted Reliability Growth Analysis (RGA) model
#' Currently only supports the Crow-AMSAA model.
#'
#' @param x An object of class \code{rga}.
#' @param main Title of the plot.
#' @param ... Additional arguments passed to \code{plot()}.
#' @examples
#' times <- c(5, 10, 15, 20, 25)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' ppplot.rga(fit)
#' @returns A P-P plot comparing empirical and theoretical CDFs.
#' @family goodness-of-fit
#' @importFrom stats lsfit
#' @export
ppplot.rga <- function(x,
                       main = "P-P Plot",
                       ...) {
  if (!inherits(x, "rga")) stop("Input must be of class 'rga'.")

  # Extract parameters (Crow-AMSAA case)
  beta <- if (is.list(x$betas)) x$betas$log_times[1, "Est."] else x$betas[1]
  lambda <- if (is.matrix(x$lambdas)) x$lambdas[1, "Est."] else x$lambdas[1]

  # Observed cumulative times
  times <- sort(
    exp(x$model$model$log_times)
  )
  n <- length(times)

  # Empirical CDF (median rank plotting position)
  emp_cdf <- (1:n) / (n + 1)

  # Theoretical CDF from Weibull model
  theo_cdf <- lambda * times^beta

  # Plot with probability scaling
  plot(theo_cdf, emp_cdf,
       main = main,
       xlab = "Theoretical CDF",
       ylab = "Empirical CDF",
       ...)

  # 45-degree reference line
  abline(lsfit(theo_cdf, emp_cdf))
}

