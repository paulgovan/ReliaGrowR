#' Mean Cumulative Function for Repairable Systems.
#'
#' Computes the non-parametric Mean Cumulative Function (MCF) for recurrent
#' event data from one or more repairable systems, using the Nelson-Aalen
#' estimator. The MCF estimates the expected cumulative number of events per
#' system as a function of time, properly accounting for system exposure
#' (observation windows).
#'
#' @srrstats {G1.0} The Nelson-Aalen estimator for recurrent events is
#'   described in Nelson (2003) <doi:10.1198/004017002188618563>.
#' @srrstats {G1.3} All statistical terminology is explicitly defined in the
#'   documentation.
#' @srrstats {G1.4} \code{roxygen2} documentation is used to document all
#'   functions.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.7} Both vectors and data frames are accepted as input.
#' @srrstats {G2.13} The function checks for missing data and errors if any
#'   is found.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.15} The function checks for missing data and errors if any
#'   is found.
#' @srrstats {G2.16} The function checks for NA and NaN values and errors if
#'   any are found.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#'
#' @param id A vector of system/unit identifiers. Each unique value represents
#'   a distinct system. Ignored if \code{data} is provided.
#' @param time A numeric vector of event or censoring times. Must be positive
#'   and finite. Ignored if \code{data} is provided.
#' @param event An optional numeric vector of event indicators: 1 for an event,
#'   0 for censoring (end of observation). If \code{NULL} (default), all
#'   observations are treated as events.
#' @param end_time An optional named numeric vector of end-of-observation times
#'   per system, where names correspond to system identifiers. This defines
#'   the actual exposure window for each system. When provided, a system
#'   remains in the risk set until its \code{end_time}, even if its last event
#'   occurred earlier. If \code{NULL} (default), the end of observation is
#'   inferred as the maximum time recorded for each system (from both events
#'   and censoring records).
#' @param data An optional data frame containing columns named \code{id},
#'   \code{time}, and optionally \code{event} and \code{end_time}.
#' @param conf_level Confidence level for bounds (default 0.95).
#' @family Repairable Systems Analysis
#' @return An object of class \code{mcf} containing:
#' \item{time}{Unique event times.}
#' \item{mcf}{MCF values at each event time.}
#' \item{variance}{Variance estimates at each event time.}
#' \item{lower_bounds}{Lower confidence bounds.}
#' \item{upper_bounds}{Upper confidence bounds.}
#' \item{conf_level}{Confidence level used.}
#' \item{n_systems}{Number of distinct systems.}
#' \item{n_events}{Total number of events.}
#' \item{end_times}{Named vector of end-of-observation times per system.}
#'
#' @details
#' The MCF at time \eqn{t} is estimated as:
#' \deqn{\hat{M}(t) = \sum_{t_j \le t} \frac{d_j}{n_j}}
#' where \eqn{d_j} is the number of events at time \eqn{t_j} and \eqn{n_j} is
#' the number of systems still under observation at \eqn{t_j}. Variance is
#' estimated as \eqn{\hat{V}(t) = \sum_{t_j \le t} d_j / n_j^2}.
#'
#' The risk set \eqn{n_j} is determined by each system's **exposure window**.
#' A system is considered at risk at time \eqn{t_j} if its end-of-observation
#' time (from \code{end_time}, censoring records, or last event) is
#' \eqn{\ge t_j}. Specifying \code{end_time} is important when systems were
#' observed beyond their last event -- without it, the MCF may overestimate
#' the true recurrence rate because systems with no late events are assumed to
#' have left observation at their last event time.
#'
#' @examples
#' # Basic usage (end of observation inferred from last event)
#' id <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
#' time <- c(100, 300, 500, 150, 400, 50, 200, 350, 600)
#' result <- mcf(id, time)
#' print(result)
#' plot(result, main = "Mean Cumulative Function")
#'
#' # With explicit end-of-observation times (exposure-adjusted)
#' end_time <- c("1" = 800, "2" = 800, "3" = 800)
#' result2 <- mcf(id, time, end_time = end_time)
#' print(result2)
#'
#' df <- data.frame(id = id, time = time)
#' result3 <- mcf(data = df)
#' print(result3)
#' @importFrom stats qnorm
#' @importFrom graphics plot lines legend
#' @export
mcf <- function(id = NULL, time = NULL, event = NULL, end_time = NULL,
                data = NULL, conf_level = 0.95) {

  # Extract from data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame.")
    }
    if (!"id" %in% names(data)) {
      stop("'data' must contain a column named 'id'.")
    }
    if (!"time" %in% names(data)) {
      stop("'data' must contain a column named 'time'.")
    }
    id <- data$id
    time <- data$time
    if ("event" %in% names(data)) {
      event <- data$event
    }
    if ("end_time" %in% names(data) && is.null(end_time)) {
      # Extract per-system end_time from data frame: take max end_time per id
      et_df <- stats::aggregate(data$end_time, by = list(id = data$id), FUN = max)
      end_time <- stats::setNames(et_df$x, et_df$id)
    }
  }

  # Validate id
  if (is.null(id)) stop("'id' must be provided.")
  if (length(id) == 0) stop("'id' cannot be empty.")

  # Validate time
  if (is.null(time)) stop("'time' must be provided.")
  if (!is.numeric(time) || !is.vector(time)) {
    stop("'time' must be a numeric vector.")
  }
  if (length(time) == 0) stop("'time' cannot be empty.")
  if (any(is.na(time)) || any(is.nan(time))) {
    stop("'time' contains missing (NA) or NaN values.")
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop("All values in 'time' must be finite and > 0.")
  }
  if (length(id) != length(time)) {
    stop("'id' and 'time' must have the same length.")
  }

  # Validate event
  if (is.null(event)) {
    event <- rep(1, length(time))
  }
  if (!is.numeric(event) || !is.vector(event)) {
    stop("'event' must be a numeric vector.")
  }
  if (length(event) != length(time)) {
    stop("'event' and 'time' must have the same length.")
  }
  if (any(is.na(event)) || any(is.nan(event))) {
    stop("'event' contains missing (NA) or NaN values.")
  }
  if (!all(event %in% c(0, 1))) {
    stop("'event' must contain only 0 (censored) or 1 (event).")
  }

  # Validate end_time
  if (!is.null(end_time)) {
    if (!is.numeric(end_time)) {
      stop("'end_time' must be a numeric vector.")
    }
    if (any(is.na(end_time)) || any(is.nan(end_time))) {
      stop("'end_time' contains missing (NA) or NaN values.")
    }
    if (any(!is.finite(end_time)) || any(end_time <= 0)) {
      stop("All values in 'end_time' must be finite and > 0.")
    }
  }

  # Validate conf_level
  if (!is.numeric(conf_level) || length(conf_level) != 1) {
    stop("'conf_level' must be a single numeric value.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1 (exclusive).")
  }

  # Compute MCF using Nelson-Aalen estimator
  unique_ids <- unique(id)
  n_systems <- length(unique_ids)
  n_events <- sum(event)

  # Determine the end-of-observation time per system
  # Priority: explicit end_time > inferred from data (max observed time)
  inferred_max <- c(tapply(time, id, max))

  if (!is.null(end_time)) {
    # Match end_time to system IDs
    end_time_names <- names(end_time)
    if (is.null(end_time_names)) {
      # Unnamed: assume same order as unique_ids
      if (length(end_time) != n_systems) {
        stop("Unnamed 'end_time' must have one value per system (length ",
             n_systems, ").")
      }
      obs_end <- stats::setNames(end_time, as.character(unique_ids))
    } else {
      obs_end <- inferred_max
      matched <- intersect(as.character(names(obs_end)), end_time_names)
      obs_end[matched] <- pmax(obs_end[matched], end_time[matched])
      # For systems in end_time not in data, ignore silently
      unmatched_et <- setdiff(end_time_names, as.character(names(obs_end)))
      if (length(unmatched_et) > 0) {
        # Systems in end_time but not in event data: still add as at-risk
        for (nm in unmatched_et) {
          obs_end[nm] <- end_time[nm]
          n_systems <- n_systems + 1
        }
      }
    }
  } else {
    obs_end <- inferred_max
  }

  # Get event times only (exclude censoring)
  event_times <- time[event == 1]
  unique_event_times <- sort(unique(event_times))

  mcf_vals <- numeric(length(unique_event_times))
  var_vals <- numeric(length(unique_event_times))

  for (j in seq_along(unique_event_times)) {
    t_j <- unique_event_times[j]
    # Number of events at t_j
    d_j <- sum(event_times == t_j)
    # Number of systems still under observation at t_j
    n_j <- sum(obs_end >= t_j)
    if (n_j > 0) {
      mcf_vals[j] <- d_j / n_j
      var_vals[j] <- d_j / n_j^2
    }
  }

  # Cumulative sums
  mcf_cum <- cumsum(mcf_vals)
  var_cum <- cumsum(var_vals)

  # Confidence bounds
  z_val <- stats::qnorm(1 - (1 - conf_level) / 2)
  se <- sqrt(var_cum)
  lower <- pmax(mcf_cum - z_val * se, 0)
  upper <- mcf_cum + z_val * se

  result <- list(
    time         = unique_event_times,
    mcf          = mcf_cum,
    variance     = var_cum,
    lower_bounds = lower,
    upper_bounds = upper,
    conf_level   = conf_level,
    n_systems    = n_systems,
    n_events     = n_events,
    end_times    = obs_end
  )
  class(result) <- "mcf"
  result
}

#' Print Method for mcf Objects.
#'
#' Prints a summary of the Mean Cumulative Function results.
#'
#' @param x An object of class \code{mcf}.
#' @param ... Additional arguments (not used).
#' @family Repairable Systems Analysis
#' @return Invisibly returns the input object.
#' @examples
#' id <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
#' time <- c(100, 300, 500, 150, 400, 50, 200, 350, 600)
#' result <- mcf(id, time)
#' print(result)
#' @export
print.mcf <- function(x, ...) {
  if (!inherits(x, "mcf")) {
    stop("'x' must be an object of class 'mcf'.")
  }

  cat("Mean Cumulative Function (MCF)\n")
  cat("------------------------------\n")
  cat(sprintf("Number of systems: %d\n", x$n_systems))
  cat(sprintf("Number of events:  %d\n", x$n_events))
  if (!is.null(x$end_times)) {
    cat(sprintf("Total exposure:    %.2f\n", sum(x$end_times)))
  }
  cat(sprintf("Confidence level:  %.0f%%\n\n", x$conf_level * 100))

  pct <- round(x$conf_level * 100)
  df <- data.frame(
    Time  = x$time,
    MCF   = round(x$mcf, 4),
    Lower = round(x$lower_bounds, 4),
    Upper = round(x$upper_bounds, 4),
    check.names = FALSE
  )
  names(df)[3] <- sprintf("Lower (%d%%)", pct)
  names(df)[4] <- sprintf("Upper (%d%%)", pct)
  print(df, row.names = FALSE)

  invisible(x)
}

#' Plot Method for mcf Objects.
#'
#' Plots the Mean Cumulative Function with optional confidence bounds.
#'
#' @param x An object of class \code{mcf}.
#' @param conf_bounds Logical; include confidence bounds (default: TRUE).
#' @param legend Logical; show the legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "topleft").
#' @param ... Additional arguments passed to \code{plot()}.
#' @family Repairable Systems Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' id <- c(1, 1, 1, 2, 2, 3, 3, 3, 3)
#' time <- c(100, 300, 500, 150, 400, 50, 200, 350, 600)
#' result <- mcf(id, time)
#' plot(result, main = "Mean Cumulative Function")
#' @importFrom graphics plot lines legend
#' @export
plot.mcf <- function(x,
                     conf_bounds = TRUE,
                     legend = TRUE,
                     legend_pos = "topleft",
                     ...) {
  if (!inherits(x, "mcf")) {
    stop("'x' must be an object of class 'mcf'.")
  }
  if (!is.logical(conf_bounds) || length(conf_bounds) != 1) {
    stop("'conf_bounds' must be a single logical value.")
  }
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value.")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string.")
  }

  # Step function: duplicate points to create steps
  n <- length(x$time)
  if (n == 0) {
    graphics::plot(0, 0, type = "n", ...)
    return(invisible(NULL))
  }

  # Plot MCF as a step function
  graphics::plot(x$time, x$mcf, type = "s", pch = 16, ...)

  if (conf_bounds) {
    graphics::lines(x$time, x$lower_bounds, type = "s", lty = 2)
    graphics::lines(x$time, x$upper_bounds, type = "s", lty = 2)
  }

  if (legend) {
    pct <- round(x$conf_level * 100)
    legend_labels <- "MCF"
    legend_lty <- 1
    legend_pch <- NA

    if (conf_bounds) {
      legend_labels <- c(legend_labels, sprintf("Conf. Bounds (%d%%)", pct))
      legend_lty <- c(legend_lty, 2)
      legend_pch <- c(legend_pch, NA)
    }

    graphics::legend(
      legend_pos,
      legend = legend_labels,
      lty = legend_lty,
      pch = legend_pch,
      bty = "n"
    )
  }

  invisible(NULL)
}
