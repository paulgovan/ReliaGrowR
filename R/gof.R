#' Goodness-of-Fit Statistics for RGA Objects
#'
#' Computes numerical goodness-of-fit statistics for a fitted Reliability Growth
#' Analysis (RGA) model using the time-transformation approach. For a
#' Crow-AMSAA (Power Law NHPP) model with parameters \eqn{\beta} and
#' \eqn{\lambda}, the transformed values
#' \eqn{W_i = (t_i / t_n)^{\beta}}
#' should follow a Uniform(0, 1) distribution if the model fits. The
#' Cramér-von Mises and Kolmogorov-Smirnov statistics are computed against
#' this null distribution.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.8} A \code{print.gof} method is provided for the \code{gof} class.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
#'
#' @param x An object for which a goodness-of-fit method is defined.
#' @param ... Additional arguments passed to methods.
#' @family goodness-of-fit
#' @return An object of class \code{gof}.
#' @examples
#' times <- c(5, 10, 15, 20, 25)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' g <- gof(fit)
#' print(g)
#' @export
gof <- function(x, ...) UseMethod("gof")

#' @rdname gof
#' @export
gof.default <- function(x, ...) {
  stop("'x' must be an object of class 'rga'.")
}

#' @rdname gof
#' @export
gof.rga <- function(x, ...) {
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }
  if (!is.null(x$breakpoints)) {
    stop("'gof.rga()' currently supports only the Crow-AMSAA model (no breakpoints).")
  }

  beta <- x$betas[1]
  if (is.null(beta) || !is.finite(beta)) {
    stop("Could not extract a finite beta parameter from the 'rga' object.")
  }

  if (!is.null(x$cum_times)) {
    times <- sort(x$cum_times)
  } else {
    times <- sort(exp(x$model$model$log_times))
  }
  n <- length(times)
  if (n < 2) {
    stop("At least 2 observations are required for goodness-of-fit testing.")
  }

  # Time-transformed values: W_i = (t_i / t_n)^beta, should be Uniform(0,1)
  w <- (times / max(times))^beta

  i <- seq_len(n)
  cvm_stat <- sum((w - (2 * i - 1) / (2 * n))^2) + 1 / (12 * n)
  ks_stat  <- max(abs(w - i / n))

  result <- list(
    cvm        = cvm_stat,
    ks         = ks_stat,
    n          = n,
    model_type = "Crow-AMSAA"
  )
  class(result) <- "gof"
  result
}

#' Print Method for gof Objects
#'
#' Prints a summary of goodness-of-fit statistics.
#'
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all functions.
#' @srrstats {G2.8} This method is provided for the \code{gof} class.
#'
#' @param x An object of class \code{gof}.
#' @param ... Additional arguments (not used).
#' @family goodness-of-fit
#' @return Invisibly returns the input object.
#' @examples
#' times <- c(5, 10, 15, 20, 25)
#' failures <- c(1, 2, 1, 3, 2)
#' fit <- rga(times, failures)
#' g <- gof(fit)
#' print(g)
#' @export
print.gof <- function(x, ...) {
  if (!inherits(x, "gof")) {
    stop("'x' must be an object of class 'gof'.")
  }
  cat(sprintf("Goodness-of-Fit Statistics (%s)\n", x$model_type))
  cat(paste(rep("-", 42), collapse = ""), "\n")
  cat(sprintf("n (observations): %d\n\n", x$n))
  cat(sprintf("Cramer-von Mises statistic (W^2): %.5f\n", x$cvm))
  cat(sprintf("Kolmogorov-Smirnov statistic (D):  %.5f\n", x$ks))
  cat("\nSmaller values indicate a better fit.\n")
  cat("W_i = (t_i / t_n)^beta should be Uniform(0,1) under H0.\n")
  invisible(x)
}

#' Q-Q Plot for RGA Objects
#'
#' This function creates a Q-Q plot for a fitted Reliability Growth Analysis (RGA) model
#' Currently only supports the Crow-AMSAA model. A Q-Q plot compares the quantiles of
#' the empirical data to the quantiles of the theoretical distribution specified by the model.
#' If the model fits well, the points should fall approximately along a straight line.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.8} Sub-functions `qqplot.rga` and `ppplot.rga` are provided for the `rga` class.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
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
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }
  if (!is.character(main) || length(main) != 1) {
    stop("'main' must be a single character string.")
  }

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
    ...
  )
  abline(lsfit(theo_quants, times))
}

#' P-P Plot for RGA Objects
#'
#' This function creates a P-P plot for a fitted Reliability Growth Analysis (RGA)
#' model. Currently only supports the Crow-AMSAA model. A P-P plot compares the
#' empirical cumulative distribution function (CDF) to the theoretical CDF specified
#' by the model. If the model fits well, the points should fall approximately along a straight line.
#'
#' @srrstats {G1.2} The Life Cycle Statement is in the CONTRIBUTING.md file.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G1.4} `roxygen2`](https://roxygen2.r-lib.org/) documentation is used
#' to document all functions.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the documentation.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.2} Multivariate inputs are prohibited where only univariate are allowed.
#' @srrstats {G2.8} Sub-functions `qqplot.rga` and `ppplot.rga` are provided for the `rga` class.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.
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
  # Input validation
  if (!inherits(x, "rga")) {
    stop("'x' must be an object of class 'rga'.")
  }
  if (!is.character(main) || length(main) != 1) {
    stop("'main' must be a single character string.")
  }

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
    ...
  )

  # 45-degree reference line
  abline(lsfit(theo_cdf, emp_cdf))
}
