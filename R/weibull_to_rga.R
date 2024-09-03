#' Convert Weibull Data to Reliability Growth Data
#'
#' @param failures A vector of failure times.
#' @param suspensions A vector of suspension (censoring) times.
#' @return A data frame with cumulative times and failures suitable for reliability growth analysis.
#' @examples
#' failures <- c(100, 200, 300, 400)
#' suspensions <- c(250, 350, 450)
#' result <- weibull_to_rga(failures, suspensions)
#' print(result)
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

  # Calculate cumulative time considering both failures and suspensions
  data$CumulativeTime <- cumsum(data$Time)

  # Create a cumulative failure count, but only increment for failures
  data$Failures <- cumsum(data$Type == "Failure")

  # Filter out suspensions, keeping only failures in the final result
  data_filtered <- subset(data, Type == "Failure")

  # Select relevant columns for output
  result <- data_filtered[, c("CumulativeTime", "Failures")]

  return(result)
}
