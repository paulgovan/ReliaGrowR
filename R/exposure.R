#' Exposure Analysis for Repairable Systems.
#'
#' Computes exposure (total operating time at risk) across one or more
#' repairable systems as a function of time. Exposure is defined as the
#' total accumulated observation time summed across all systems still under
#' observation. The function also computes the number of systems at risk
#' and the event rate (events per unit exposure) at each event time.
#'
#' @srrstats {G1.0} Exposure analysis for repairable systems is described
#'   in Meeker and Escobar (1998) <doi:10.1002/9781118032985>.
#' @srrstats {G1.3} All statistical terminology is explicitly defined.
#' @srrstats {G1.4} \code{roxygen2} documentation is used.
#' @srrstats {G2.0} Inputs are validated for length.
#' @srrstats {G2.1} Inputs are validated for type.
#' @srrstats {G2.7} Both vectors and data frames are accepted as input.
#' @srrstats {G2.13} The function checks for missing data.
#' @srrstats {G2.14a} Missing data results in an error.
#' @srrstats {G2.16} NA and NaN values result in an error.
#' @srrstats {G5.2a} Every message produced by \code{stop()} is unique.
#'
#' @param id A vector of system/unit identifiers. Each unique value
#'   represents a distinct system.
#' @param time A numeric vector of event or censoring times. Must be
#'   positive and finite.
#' @param event An optional numeric vector of event indicators: 1 for an
#'   event, 0 for censoring (end of observation). If \code{NULL} (default),
#'   all observations are treated as events, and the maximum time per system
#'   is treated as the end of observation.
#' @param data An optional data frame containing columns named \code{id},
#'   \code{time}, and optionally \code{event}.
#' @family Repairable Systems Analysis
#' @return An object of class \code{exposure} containing:
#' \item{time}{Sorted unique event times (excluding censoring-only times).}
#' \item{n_at_risk}{Number of systems under observation at each event time.}
#' \item{cum_exposure}{Cumulative total exposure (system-time) up to each
#'   event time.}
#' \item{cum_events}{Cumulative number of events up to each event time.}
#' \item{event_rate}{Cumulative event rate (cum_events / cum_exposure) at
#'   each event time.}
#' \item{total_exposure}{Total exposure across all systems and the full
#'   observation period.}
#' \item{total_events}{Total number of events.}
#' \item{n_systems}{Number of distinct systems.}
#' \item{end_times}{Named numeric vector of end-of-observation times per
#'   system. Can be passed directly to \code{mcf(end_time = ...)} to ensure
#'   the MCF properly accounts for system exposure.}
#'
#' @details
#' **Exposure** is the total amount of operating time during which events
#' can occur. For a fleet of \eqn{k} systems observed up to times
#' \eqn{T_1, T_2, \ldots, T_k}, the total exposure is
#' \eqn{E = \sum_{i=1}^{k} T_i}{E = T_1 + T_2 + ... + T_k}.
#'
#' The **cumulative exposure** at time \eqn{t} is
#' \eqn{E(t) = \sum_{i=1}^{k} \min(t, T_i)}{E(t) = sum of min(t, T_i)},
#' i.e., each system contributes time up to the lesser of \eqn{t} or its
#' observation end.
#'
#' The **event rate** at time \eqn{t} is the cumulative number of events
#' divided by the cumulative exposure: \eqn{r(t) = N(t) / E(t)}.
#'
#' @examples
#' id   <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
#' time <- c(100, 350, 500, 80, 300, 600, 150, 250, 400, 700)
#' result <- exposure(id, time)
#' print(result)
#' plot(result)
#'
#' # With censoring
#' id    <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
#' time  <- c(100, 350, 500, 80, 300, 400, 150, 250, 700)
#' event <- c(  1,   1,   0,  1,   1,   0,   1,   1,   1)
#' result2 <- exposure(id, time, event)
#' print(result2)
#' @importFrom stats aggregate
#' @importFrom graphics plot lines par axis mtext legend
#' @export
exposure <- function(id = NULL, time = NULL, event = NULL, data = NULL) {

  # Extract from data frame if provided
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame when provided to exposure().")
    }
    if (!"id" %in% names(data)) {
      stop("'data' must contain a column named 'id' for exposure().")
    }
    if (!"time" %in% names(data)) {
      stop("'data' must contain a column named 'time' for exposure().")
    }
    id <- data$id
    time <- data$time
    if ("event" %in% names(data)) {
      event <- data$event
    }
  }

  # Validate id
  if (is.null(id)) stop("'id' must be provided to exposure().")
  if (length(id) == 0) stop("'id' cannot be empty in exposure().")

  # Validate time
  if (is.null(time)) stop("'time' must be provided to exposure().")
  if (!is.numeric(time) || !is.vector(time)) {
    stop("'time' must be a numeric vector in exposure().")
  }
  if (length(time) == 0) stop("'time' cannot be empty in exposure().")
  if (any(is.na(time)) || any(is.nan(time))) {
    stop("'time' contains missing (NA) or NaN values in exposure().")
  }
  if (any(!is.finite(time)) || any(time <= 0)) {
    stop("All values in 'time' must be finite and > 0 in exposure().")
  }
  if (length(id) != length(time)) {
    stop("'id' and 'time' must have the same length in exposure().")
  }

  # Validate event
  if (is.null(event)) {
    event <- rep(1, length(time))
  }
  if (!is.numeric(event) || !is.vector(event)) {
    stop("'event' must be a numeric vector in exposure().")
  }
  if (length(event) != length(time)) {
    stop("'event' and 'time' must have the same length in exposure().")
  }
  if (any(is.na(event)) || any(is.nan(event))) {
    stop("'event' contains missing (NA) or NaN values in exposure().")
  }
  if (!all(event %in% c(0, 1))) {
    stop("'event' must contain only 0 (censored) or 1 (event) in exposure().")
  }

  # Core calculations
  unique_ids <- unique(id)
  n_systems <- length(unique_ids)
  total_events <- sum(event)

  # Max observation time per system (includes censoring times)
  max_times <- tapply(time, id, max)

  # Total exposure = sum of all max observation times
  total_exposure <- sum(max_times)

  # Get event times only
  event_times <- time[event == 1]
  unique_event_times <- sort(unique(event_times))

  n_times <- length(unique_event_times)
  n_at_risk <- numeric(n_times)
  cum_exposure <- numeric(n_times)
  cum_events_vec <- numeric(n_times)

  cum_ev <- 0
  for (j in seq_along(unique_event_times)) {
    t_j <- unique_event_times[j]

    # Number of systems still under observation at t_j
    n_at_risk[j] <- sum(max_times >= t_j)

    # Cumulative exposure up to t_j: each system contributes min(t_j, T_i)
    cum_exposure[j] <- sum(pmin(t_j, max_times))

    # Cumulative events up to t_j
    cum_ev <- cum_ev + sum(event_times == t_j)
    cum_events_vec[j] <- cum_ev
  }

  # Event rate = cum_events / cum_exposure
  event_rate <- ifelse(cum_exposure > 0, cum_events_vec / cum_exposure, 0)

  result <- list(
    time           = unique_event_times,
    n_at_risk      = n_at_risk,
    cum_exposure   = cum_exposure,
    cum_events     = cum_events_vec,
    event_rate     = event_rate,
    total_exposure = total_exposure,
    total_events   = total_events,
    n_systems      = n_systems,
    end_times      = max_times
  )
  class(result) <- "exposure"
  result
}


#' Print Method for exposure Objects.
#'
#' Prints a summary of the exposure analysis results.
#'
#' @param x An object of class \code{exposure}.
#' @param ... Additional arguments (not used).
#' @family Repairable Systems Analysis
#' @return Invisibly returns the input object.
#' @examples
#' id   <- c(1, 1, 2, 2)
#' time <- c(100, 200, 150, 300)
#' result <- exposure(id, time)
#' print(result)
#' @export
print.exposure <- function(x, ...) {
  if (!inherits(x, "exposure")) {
    stop("'x' must be an object of class 'exposure'.")
  }

  cat("Exposure Analysis for Repairable Systems\n")
  cat("-----------------------------------------\n")
  cat(sprintf("Number of systems:  %d\n", x$n_systems))
  cat(sprintf("Total events:       %d\n", x$total_events))
  cat(sprintf("Total exposure:     %.2f\n", x$total_exposure))
  cat(sprintf("Overall event rate: %.6f\n\n", x$total_events / x$total_exposure))

  df <- data.frame(
    Time         = x$time,
    At.Risk      = x$n_at_risk,
    Cum.Exposure = round(x$cum_exposure, 2),
    Cum.Events   = x$cum_events,
    Event.Rate   = round(x$event_rate, 6),
    check.names  = FALSE
  )
  names(df) <- c("Time", "At Risk", "Cum. Exposure", "Cum. Events", "Event Rate")
  print(df, row.names = FALSE)

  invisible(x)
}


#' Plot Method for exposure Objects.
#'
#' Produces a multi-panel plot of exposure analysis results. The default
#' layout shows cumulative exposure and cumulative events versus time
#' (top panel), the number of systems at risk over time (middle panel),
#' and the event rate over time (bottom panel). Alternatively, a single
#' \code{which} panel can be selected.
#'
#' @param x An object of class \code{exposure}.
#' @param which Character string selecting which panel(s) to plot. One of
#'   \code{"all"} (default), \code{"exposure"}, \code{"at_risk"}, or
#'   \code{"event_rate"}.
#' @param legend Logical; show the legend (default: TRUE).
#' @param legend_pos Position of the legend (default: "topleft").
#' @param ... Additional arguments passed to the underlying \code{plot()}.
#' @family Repairable Systems Analysis
#' @return Invisibly returns \code{NULL}.
#' @examples
#' id   <- c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
#' time <- c(100, 350, 500, 80, 300, 600, 150, 250, 400, 700)
#' result <- exposure(id, time)
#' plot(result)
#' plot(result, which = "exposure")
#' @importFrom graphics plot lines par axis mtext legend
#' @export
plot.exposure <- function(x,
                          which = c("all", "exposure", "at_risk", "event_rate"),
                          legend = TRUE,
                          legend_pos = "topleft",
                          ...) {
  if (!inherits(x, "exposure")) {
    stop("'x' must be an object of class 'exposure'.")
  }
  which <- match.arg(which)
  if (!is.logical(legend) || length(legend) != 1) {
    stop("'legend' must be a single logical value in plot.exposure().")
  }
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    stop("'legend_pos' must be a single character string in plot.exposure().")
  }

  if (which == "all") {
    old_par <- graphics::par(mfrow = c(3, 1), mar = c(4, 4, 2, 4))
    on.exit(graphics::par(old_par), add = TRUE)

    # Panel 1: Cumulative exposure and cumulative events
    .plot_exposure_panel(x, legend = legend, legend_pos = legend_pos, ...)

    # Panel 2: Number at risk
    .plot_at_risk_panel(x, ...)

    # Panel 3: Event rate
    .plot_event_rate_panel(x, ...)

  } else if (which == "exposure") {
    .plot_exposure_panel(x, legend = legend, legend_pos = legend_pos, ...)
  } else if (which == "at_risk") {
    .plot_at_risk_panel(x, ...)
  } else {
    .plot_event_rate_panel(x, ...)
  }

  invisible(NULL)
}


# Internal: Plot cumulative exposure and cumulative events on dual y-axes.
.plot_exposure_panel <- function(x, legend = TRUE, legend_pos = "topleft", ...) {
  # Cumulative exposure (left axis)
  graphics::plot(x$time, x$cum_exposure,
                 type = "s", lwd = 2,
                 xlab = "Time",
                 ylab = "Cumulative Exposure",
                 main = "Cumulative Exposure and Events",
                 ...)

  # Cumulative events on secondary axis
  graphics::par(new = TRUE)
  graphics::plot(x$time, x$cum_events,
                 type = "s", lty = 2, lwd = 2, col = "steelblue",
                 axes = FALSE, xlab = "", ylab = "")
  graphics::axis(side = 4, col = "steelblue", col.axis = "steelblue")
  graphics::mtext("Cumulative Events", side = 4, line = 2.5, col = "steelblue")

  if (legend) {
    graphics::legend(
      legend_pos,
      legend = c("Cum. Exposure", "Cum. Events"),
      lty = c(1, 2),
      lwd = c(2, 2),
      col = c("black", "steelblue"),
      bty = "n"
    )
  }
}


# Internal: Plot number of systems at risk over time.
.plot_at_risk_panel <- function(x, ...) {
  graphics::plot(x$time, x$n_at_risk,
                 type = "s", lwd = 2,
                 xlab = "Time",
                 ylab = "Systems at Risk",
                 main = "Number of Systems at Risk",
                 ...)
}


# Internal: Plot event rate over time.
.plot_event_rate_panel <- function(x, ...) {
  graphics::plot(x$time, x$event_rate,
                 type = "s", lwd = 2,
                 xlab = "Time",
                 ylab = "Event Rate (events / exposure)",
                 main = "Cumulative Event Rate",
                 ...)
}
