#' Simulate Failures via PPS Sampling
#'
#' Simulates which units in a non-failed population fail next using probability
#' proportional to size (PPS) sampling based on unit runtimes. Units with
#' longer runtimes have a proportionally higher probability of being selected.
#' The full fleet is returned: selected units are labelled `"Failure"` and the
#' remaining units are labelled `"Suspension"`.
#'
#' @srrstats {G1.0} This function implements probability proportional to size
#' (PPS) sampling, a standard statistical technique.
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
#'   runtimes. When provided, failure times are
#'   `runtime + Uniform(0, window)` and suspension times are
#'   `runtime + window`.
#' @return A data frame with `length(runtimes)` rows sorted ascending by
#'   `runtime`, containing:
#'   \item{index}{Integer index of the unit in `runtimes`.}
#'   \item{runtime}{Simulated event time.}
#'   \item{type}{Character; `"Failure"` for selected units, `"Suspension"` for
#'     the rest.}
#' @family data preparation
#' @examples
#' set.seed(42)
#' runtimes <- c(100, 500, 200, 800, 300)
#' result <- sim_failures(2, runtimes)
#' print(result)
#'
#' # With an observation window
#' set.seed(42)
#' result_w <- sim_failures(2, runtimes, window = 50)
#' print(result_w)
#' @export
sim_failures <- function(n, runtimes, replace = FALSE, window = NULL) {
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

  # PPS sampling
  prob <- runtimes / sum(runtimes)
  idx  <- sample(seq_along(runtimes), size = n, replace = replace, prob = prob)

  fail_idx <- unique(idx)
  susp_idx <- setdiff(seq_along(runtimes), fail_idx)

  # Compute event times
  if (is.null(window)) {
    fail_times <- runtimes[fail_idx]
    susp_times <- runtimes[susp_idx]
  } else {
    fail_times <- runtimes[fail_idx] + runif(length(fail_idx), 0, window)
    susp_times <- runtimes[susp_idx] + window
  }

  result <- data.frame(
    index   = c(fail_idx,       susp_idx),
    runtime = c(fail_times,     susp_times),
    type    = c(rep("Failure",    length(fail_idx)),
                rep("Suspension", length(susp_idx)))
  )
  result <- result[order(result$runtime), ]
  rownames(result) <- NULL

  return(result)
}
