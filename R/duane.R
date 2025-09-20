
#' Duane Analysis
#'
#' This function performs a Duane analysis (1962) <doi:10.1109/TA.1964.4319640>
#' on failure data by fitting a log-log linear regression of cumulative MTBF
#' versus cumulative time.
#'
#' @param times A numeric vector of cumulative failure times.
#' @param failures A numeric vector of the number of failures at each corresponding time in \code{times}.
#' @param conf.level Confidence level for the confidence bounds (default: \code{0.95}).
#' @family Duane functions
#'
#' @return A list of class \code{"duane"} containing:
#' \item{model}{The fitted \code{lm} object.}
#' \item{logLik}{The log-likelihood of the fitted model.}
#' \item{AIC}{Akaike Information Criterion.}
#' \item{BIC}{Bayesian Information Criterion.}
#' \item{conf.level}{The confidence level.}
#' \item{Cumulative_Time}{The cumulative operating times.}
#' \item{Cumulative_MTBF}{The cumulative mean time between failures.}
#' \item{Fitted_Values}{The fitted values on the MTBF scale.}
#' \item{Confidence_Bounds}{Matrix of fitted values and confidence bounds on the MTBF scale.}
#'
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures, conf.level = 0.90)
#' print(fit)
#'
#' @importFrom stats lm AIC BIC logLik predict
#' @export
duane <- function(times, failures, conf.level = 0.95) {
  if (length(times) != length(failures)) {
    stop("The length of 'times' and 'failures' must be equal.")
  }
  if (any(times <= 0)) stop("All values in 'times' must be > 0.")
  if (any(failures <= 0)) stop("All values in 'failures' must be > 0.")

  # Cumulative times & failures
  cum_failures <- cumsum(failures)
  cum_times <- cumsum(times)

  # cumulative MTBF
  cum_mtbf <- cum_times / cum_failures

  # linear model (log-log)
  log_cum_times <- log(cum_times)
  log_cum_mtbf <- log(cum_mtbf)
  fit <- stats::lm(log_cum_mtbf ~ log_cum_times)

  # fit stats
  aic <- stats::AIC(fit)
  bic <- stats::BIC(fit)
  loglik <- as.numeric(stats::logLik(fit))

  # fitted values back-transformed
  fitted_values <- exp(predict(fit))

  # always compute CI
  pred <- predict(fit, interval = "confidence", level = conf.level)
  ci_bounds <- exp(pred)

  result <- list(
    model = fit,
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    conf.level = conf.level,
    Cumulative_Time = cum_times,
    Cumulative_MTBF = cum_mtbf,
    Fitted_Values = fitted_values,
    Confidence_Bounds = ci_bounds
  )
  class(result) <- "duane"
  return(result)
}

#' Print method for duane objects.
#'
#' This function prints a summary of the Duane analysis result.
#'
#' @param x An object of class "duane" returned by the duane_plot function.
#' @param ... Additional arguments (not used).
#' @family Duane functions
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures)
#' print(fit)
#' @return Invisibly returns the input object.
#'
#' @export
print.duane <- function(x, ...) {
  cat("Duane Analysis Result\n")
  cat("----------------------\n")
  cat("Linear model (log-log scale): log(MTBF) ~ log(Time)\n")

  # Coefficient estimates and standard errors
  smry <- summary(x$model)
  coefs <- smry$coefficients

  cat("\nCoefficients:\n")
  print(coefs[, c("Estimate", "Std. Error")])

  # Model fit statistics
  cat(sprintf("\nLog-likelihood: %.2f", x$logLik))
  cat(sprintf("\nAIC: %.2f, BIC: %.2f", x$AIC, x$BIC))

  # Confidence level, if used
  if (!is.null(x$conf.level)) {
    cat(sprintf("\nConfidence level: %.1f%%", 100 * x$conf.level))
  }

  cat("\n")
  invisible(x)
}

#' Plot Method for Duane Analysis
#'
#' Generates a Duane plot (log-log or linear scale) with fitted regression line
#' and optional confidence bounds.
#'
#' @param x An object of class \code{"duane"}.
#' @param log Logical; whether to use logarithmic scales for axes (default: \code{TRUE}).
#' @param conf.int Logical; whether to plot confidence bounds (default: \code{TRUE}).
#' @param legend Logical; whether to include a legend (default: TRUE).
#' @param legend.pos Position of the legend (default: "topleft").
#' @param ... Further arguments passed to \code{plot()}.
#' @family Duane functions
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' times <- c(100, 200, 300, 400, 500)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- duane(times, failures)
#' plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF")
#' @importFrom graphics legend lines plot
#' @export
plot.duane <- function(x,
                       log = TRUE,
                       conf.int = TRUE,
                       legend = TRUE,
                       legend.pos = "topleft",
                       ...) {
  if (!inherits(x, "duane")) stop("Input must be an object of class 'duane'.")

  cum_time <- x$Cumulative_Time
  cum_mtbf <- x$Cumulative_MTBF

  plot_args <- list(
    x = cum_time,
    y = cum_mtbf,
    pch = 16,
    ...
  )
  if (log) plot_args$log <- "xy"
  do.call(graphics::plot, plot_args)

  # fitted line
  graphics::lines(cum_time, x$Fitted_Values, lty = 1)

  # confidence bounds (optional)
  if (conf.int && !is.null(x$Confidence_Bounds)) {
    graphics::lines(cum_time, x$Confidence_Bounds[, "lwr"], lty = 2)
    graphics::lines(cum_time, x$Confidence_Bounds[, "upr"], lty = 2)
  }

  # legend
  if (legend) {
    legend_items <- c("Observed", "Fitted Line")
    pch <- c(16, NA)
    lty <- c(NA, 1)

    if (conf.int && !is.null(x$Confidence_Bounds)) {
      legend_items <- c(legend_items, "Confidence Bounds")
      pch <- c(pch, NA)
      lty <- c(lty, 2)
    }

    graphics::legend(legend.pos, legend = legend_items,
                     pch = pch, lty = lty, bty = "n")
  }

  invisible(NULL)
}
