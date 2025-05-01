#' Weibull to RGA
#'
#' This function converts Weibull data (failure and suspension times) into a format
#' suitable for reliability growth analysis (RGA).
#'
#' @param failures A vector of failure times.
#' @param suspensions A vector of suspension (censoring) times.
#' @return A data frame with times and failure counts suitable for reliability growth analysis.
#' @examples
#' failures <- c(100, 200, 200, 400)
#' suspensions <- c(250, 350, 450)
#' result <- weibull_to_rga(failures, suspensions)
#' print(result)
#' @importFrom stats aggregate
#' @export

weibull_to_rga <- function(failures, suspensions = NULL) {

  # Check if the failures are valid
  if (!is.numeric(failures) || any(failures <= 0)) {
    stop("Error: `failures` must be a numeric vector with positive values.")
  }

  # Check if the suspensions are valid if provided
  if (!is.null(suspensions)) {
    if (!is.numeric(suspensions) || any(suspensions <= 0)) {
      stop("Error: suspensions must be a numeric vector with positive values.")
    }
  }

  # Combine failure and suspension times
  all_times <- c(failures, suspensions)
  all_types <- c(rep("Failure", length(failures)), rep("Suspension", length(suspensions)))

  # Create a data frame
  data <- data.frame(Time = all_times, Type = all_types)

  # Sort the data by time
  data <- data[order(data$Time), ]

  # Separate cumulative time and failure counts
  data$CumulativeTime <- cumsum(data$Time)

  # Create the failure counts for each unique time
  failure_counts <- stats::aggregate(Type ~ Time, data = data, FUN = function(x) sum(x == "Failure"))

  # Merge the cumulative time with the failure counts
  result <- merge(failure_counts, data[, c("Time", "CumulativeTime")], by = "Time")

  # Remove duplicates due to merging
  result <- result[!duplicated(result$Time), ]

  # Rename the failure count column
  colnames(result)[colnames(result) == "Type"] <- "Failures"

  # Filter out rows where Failures are 0 (which are suspensions)
  result <- result[result$Failures > 0, ]

  # Select relevant columns for output
  result <- result[, c("CumulativeTime", "Failures")]

  return(result)
}
