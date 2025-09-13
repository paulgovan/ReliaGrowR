#' Weibull to RGA
#'
#' Converts Weibull data (failure, suspension, and interval-censored times)
#' into a format suitable for reliability growth analysis (RGA).
#'
#' @param failures A numeric vector of exact failure times.
#' @param suspensions A numeric vector of suspension (right-censored) times.
#' @param interval_starts A numeric vector of interval start times (lower bound of censoring).
#' @param interval_ends A numeric vector of interval end times (upper bound of censoring).
#' @return A data frame with cumulative time and failure counts suitable for RGA.
#' @examples
#' failures <- c(100, 200, 200, 400)
#' suspensions <- c(250, 350, 450)
#' interval_starts <- c(150, 300)
#' interval_ends <- c(180, 320)
#' result <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)
#' print(result)
#' @importFrom stats aggregate
#' @export
weibull_to_rga <- function(failures,
                           suspensions = NULL,
                           interval_starts = NULL,
                           interval_ends = NULL) {

  # Validation
  if (!is.numeric(failures) || any(failures <= 0)) {
    stop("Failures` must be a numeric vector with positive values.")
  }

  if (!is.null(suspensions)) {
    if (!is.numeric(suspensions) || any(suspensions <= 0)) {
      stop("Suspensions` must be a numeric vector with positive values.")
    }
  }

  if (!is.null(interval_starts) || !is.null(interval_ends)) {
    if (is.null(interval_starts) || is.null(interval_ends)) {
      stop("Both `interval_starts` and `interval_ends` must be provided together.")
    }
    if (!is.numeric(interval_starts) || !is.numeric(interval_ends)) {
      stop("Interval bounds must be numeric vectors.")
    }
    if (length(interval_starts) != length(interval_ends)) {
      stop("`interval_starts` and `interval_ends` must have the same length.")
    }
    if (any(interval_starts <= 0) || any(interval_ends <= 0)) {
      stop("Interval bounds must be positive.")
    }
    if (any(interval_starts >= interval_ends)) {
      stop("Each interval start must be strictly less than its corresponding end.")
    }
  }

  # Data prep
  all_times <- c(failures, suspensions)
  all_types <- c(rep("Failure", length(failures)),
                 rep("Suspension", length(suspensions)))

  # Handle interval-censored data (approximate failures at midpoint)
  if (!is.null(interval_starts)) {
    midpoints <- (interval_starts + interval_ends) / 2
    all_times <- c(all_times, midpoints)
    all_types <- c(all_types, rep("IntervalFailure", length(midpoints)))
  }

  data <- data.frame(Time = all_times, Type = all_types)
  data <- data[order(data$Time), ]

  # Cumulative time is just running sum of times in order
  data$CumulativeTime <- cumsum(data$Time)

  # Failure counts by time (Failures + IntervalFailures)
  failure_counts <- stats::aggregate(Type ~ Time, data = data,
                                     FUN = function(x) sum(x %in% c("Failure", "IntervalFailure")))

  result <- merge(failure_counts, data[, c("Time", "CumulativeTime")], by = "Time")
  result <- result[!duplicated(result$Time), ]
  colnames(result)[colnames(result) == "Type"] <- "Failures"

  # Keep only rows where there were actual failures
  result <- result[result$Failures > 0, ]

  # Final columns
  result <- result[, c("CumulativeTime", "Failures")]

  return(result)
}
