.expected_failures_weibull <- function(eta, runtimes, window, beta) {
  delta_h <- ((runtimes + window) / eta)^beta - (runtimes / eta)^beta
  sum(1 - exp(-delta_h))
}

.solve_weibull_eta <- function(target_failures, runtimes, window, beta) {
  target_failures <- min(target_failures, length(runtimes))

  objective <- function(log_eta) {
    .expected_failures_weibull(exp(log_eta), runtimes, window, beta) - target_failures
  }

  lower <- log(max(min(runtimes) * 1e-6, 1e-8))
  upper <- log(max(runtimes + window) * 1e6)

  for (i in seq_len(25)) {
    if (objective(lower) >= 0) {
      break
    }
    lower <- lower - 2
  }
  for (i in seq_len(25)) {
    if (objective(upper) <= 0) {
      break
    }
    upper <- upper + 2
  }

  exp(stats::uniroot(objective, c(lower, upper))$root)
}

#' Simulate Failures from a Conditional Weibull Model
#'
#' Simulates which units in a non-failed population fail next by using a
#' Weibull life model conditional on each unit's current runtime. When a
#' positive `window` is supplied, the function calibrates a Weibull scale
#' parameter (unless provided directly) so that the expected number of failures
#' within the window matches `n`. Units are then sampled with probability
#' proportional to their conditional Weibull failure probability over the
#' window, and failure times are drawn from the truncated conditional Weibull
#' distribution. The full fleet is returned: selected units are labelled
#' `"Failure"` and the remaining units are labelled `"Suspension"`.
#'
#' When `window = NULL`, the function returns the current fleet state at the
#' supplied runtimes. In this case, failing units are selected using relative
#' Weibull hazard weights implied by `beta`.
#'
#' @srrstats {G1.0} This function implements conditional Weibull simulation,
#' a standard reliability-modeling technique.
#' @srrstats {G1.4} [`roxygen2`](https://roxygen2.r-lib.org/) documentation is
#' used to document all functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.13} The function checks for missing data and errors.
#' @srrstats {G5.2} Unit tests demonstrate error messages.
#' @srrstats {G5.4} Unit tests include correctness tests with fixed data.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.6} Unit tests include parameter recovery checks.
#'
#' @param n Positive integer. Number of failures to simulate.
#' @param runtimes Numeric vector of positive values. The current operating
#'   runtime of each unit in the non-failed population.
#' @param replace Logical scalar. If `TRUE`, sampling is done with replacement
#'   (a unit may be selected more than once). Default is `FALSE`.
#' @param window `NULL` or a single positive numeric. The width of the
#'   observation window. When `NULL` (default), event times equal current
#'   runtimes. When provided, failure times are sampled from the conditional
#'   Weibull distribution over `(runtime, runtime + window]`, and suspension
#'   times are `runtime + window`.
#' @param beta Positive numeric scalar. Weibull shape parameter used to model
#'   the age-dependent hazard. Defaults to `1`, corresponding to an
#'   exponential-process assumption.
#' @param eta `NULL` or a single positive numeric. Weibull scale parameter.
#'   When `NULL` and `window` is supplied, the scale is calibrated so that the
#'   expected number of failures across the fleet during the window matches
#'   `n`.
#' @return A data frame with `length(runtimes)` rows sorted ascending by
#'   `runtime`, containing:
#'   \item{index}{Integer index of the unit in `runtimes`.}
#'   \item{runtime}{Simulated event time.}
#'   \item{type}{Character; `"Failure"` for selected units, `"Suspension"` for
#'     the rest.}
#'   The returned object also carries attributes `weibull_beta` and
#'   `weibull_eta` describing the Weibull parameters used for the simulation.
#' @family data preparation
#' @examples
#' set.seed(42)
#' runtimes <- c(100, 500, 200, 800, 300)
#' result <- sim_failures(2, runtimes, beta = 1.5)
#' print(result)
#'
#' # With an observation window
#' set.seed(42)
#' result_w <- sim_failures(2, runtimes, window = 50, beta = 1.5)
#' print(result_w)
#' @importFrom stats runif uniroot
#' @export
sim_failures <- function(n, runtimes, replace = FALSE, window = NULL,
                         beta = 1, eta = NULL) {
  # Validate n
  if (!is.numeric(n) || length(n) != 1) {
    stop("'n' must be a single numeric value.")
  }
  if (is.na(n) || is.nan(n) || !is.finite(n)) {
    stop("'n' must be a finite positive integer.")
  }
  if (n != floor(n) || n < 1) {
    stop("'n' must be a positive integer.")
  }

  # Validate runtimes
  if (!is.numeric(runtimes)) {
    stop("'runtimes' must be a numeric vector.")
  }
  if (length(runtimes) == 0) {
    stop("'runtimes' cannot be empty.")
  }
  if (any(is.na(runtimes)) || any(is.nan(runtimes))) {
    stop("'runtimes' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(runtimes)) || any(runtimes <= 0)) {
    stop("All values in 'runtimes' must be finite and > 0.")
  }

  # Validate replace
  if (!is.logical(replace) || length(replace) != 1 || is.na(replace)) {
    stop("'replace' must be a single logical value (TRUE or FALSE).")
  }

  # Validate n vs length(runtimes) when sampling without replacement
  if (!replace && n > length(runtimes)) {
    stop("'n' cannot exceed the number of units in 'runtimes' when replace = FALSE.")
  }

  # Validate beta
  if (!is.numeric(beta) || length(beta) != 1) {
    stop("'beta' must be a single numeric value.")
  }
  if (is.na(beta) || is.nan(beta) || !is.finite(beta)) {
    stop("'beta' must be a finite positive numeric value.")
  }
  if (beta <= 0) {
    stop("'beta' must be a positive numeric value.")
  }

  # Validate window
  if (!is.null(window)) {
    if (!is.numeric(window) || length(window) != 1) {
      stop("'window' must be a single positive numeric value.")
    }
    if (is.na(window) || is.nan(window) || !is.finite(window)) {
      stop("'window' must be a finite positive numeric value.")
    }
    if (window <= 0) {
      stop("'window' must be a positive numeric value.")
    }
  }

  # Validate eta
  if (!is.null(eta)) {
    if (!is.numeric(eta) || length(eta) != 1) {
      stop("'eta' must be a single positive numeric value.")
    }
    if (is.na(eta) || is.nan(eta) || !is.finite(eta)) {
      stop("'eta' must be a finite positive numeric value.")
    }
    if (eta <= 0) {
      stop("'eta' must be a positive numeric value.")
    }
  }

  target_failures <- min(n, length(runtimes))

  if (is.null(window)) {
    # Without a forward window, return the current fleet state while weighting
    # selection by the relative Weibull hazard implied by beta.
    prob <- runtimes^(beta - 1)
    if (any(!is.finite(prob)) || sum(prob) <= 0) {
      prob <- rep(1, length(runtimes))
    }
    eta_used <- eta
  } else {
    eta_used <- if (is.null(eta)) {
      .solve_weibull_eta(target_failures, runtimes, window, beta)
    } else {
      eta
    }
    delta_h <- ((runtimes + window) / eta_used)^beta - (runtimes / eta_used)^beta
    prob <- 1 - exp(-delta_h)
    if (any(!is.finite(prob)) || sum(prob) <= 0) {
      stop("Unable to compute valid Weibull failure probabilities from 'runtimes'.")
    }
  }

  idx <- sample(seq_along(runtimes), size = n, replace = replace, prob = prob)

  fail_idx <- unique(idx)
  susp_idx <- setdiff(seq_along(runtimes), fail_idx)

  # Compute event times
  if (is.null(window)) {
    fail_times <- runtimes[fail_idx]
    susp_times <- runtimes[susp_idx]
  } else {
    fail_runtimes <- runtimes[fail_idx]
    h_start <- (fail_runtimes / eta_used)^beta
    delta_fail <- ((fail_runtimes + window) / eta_used)^beta - h_start
    u <- stats::runif(length(fail_idx))
    fail_times <- eta_used * (h_start - log1p(-u * (1 - exp(-delta_fail))))^(1 / beta)
    susp_times <- runtimes[susp_idx] + window
  }

  result <- data.frame(
    index = c(fail_idx, susp_idx),
    runtime = c(fail_times, susp_times),
    type = c(
      rep("Failure", length(fail_idx)),
      rep("Suspension", length(susp_idx))
    )
  )
  result <- result[order(result$runtime), ]
  rownames(result) <- NULL
  attr(result, "weibull_beta") <- beta
  attr(result, "weibull_eta") <- eta_used

  return(result)
}
