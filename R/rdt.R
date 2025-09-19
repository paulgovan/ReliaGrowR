#' Reliability Demonstration Test (RDT) Plan Calculator
#'
#' This function calculates the required test time or sample size for a Reliability
#' Demonstration Test (RDT) based on specified reliability, mission time, confidence
#' level, and Weibull shape parameter.
#'
#' @param target Required reliability at mission time (0 < target < 1).
#' @param mission_time Mission duration (time units).
#' @param conf_level Desired confidence level (e.g., 0.9 for 90% confidence).
#' @param beta Weibull shape parameter (beta=1 corresponds to exponential distribution).
#' @param n Sample size (optional, supply if solving for test_time).
#' @param test_time Test time per unit (optional, supply if solving for n).
#' @return The function returns an object of class `rdt` that contains:
#' \item{Distribution}{Type of distribution used (Exponential or Weibull).}
#' \item{Beta}{Weibull shape parameter.}
#' \item{Target_Reliability}{Specified target reliability.}
#' \item{Mission_Time}{Specified mission time.}
#' \item{Required_Test_Time}{Calculated required test time (if n is provided).}
#' \item{Input_Sample_Size}{Provided sample size (if test_time is calculated).}
#' \item{Required_Sample_Size}{Calculated required sample size (if test_time is provided).}
#' \item{Input_Test_Time}{Provided test time (if n is calculated).}
#'
#' @examples
#' #' # Example 1: Calculate required test time
#' plan1 <- rdt(target=0.9, mission_time=1000, conf_level=0.9, beta=1, n=10)
#' print(plan1)
#' # Example 2: Calculate required sample size
#' plan2 <- rdt(target=0.9, mission_time=1000, conf_level=0.9, beta=1, test_time=2000)
#' print(plan2)
#' @export
rdt <- function(target, mission_time, conf_level,
                     beta = 1, n = NULL, test_time = NULL) {

  # Input validation
  if (target <= 0 || target >= 1) {
    stop("Target reliability must be between 0 and 1 (exclusive).")
  }
  if (mission_time <= 0) {
    stop("Mission time must be greater than 0.")
  }
  if (conf_level <= 0 || conf_level >= 1) {
    stop("Confidence level must be between 0 and 1 (exclusive).")
  }
  if (beta <= 0) {
    stop("Shape parameter beta must be greater than 0.")
  }
  if (!is.null(n) && !is.null(test_time)) {
    stop("Please provide only one of n (sample size) or test_time (test time), not both.")
  }
  if (is.null(n) && is.null(test_time)) {
    stop("Please provide one of n (sample size) or test_time (test time).")
  }
  if (!is.null(n) && (n <= 0 || n != floor(n))) {
    stop("Sample size n must be a positive integer.")
  }

  # Scale parameter under H0 (Weibull with specified beta)
  eta0 <- mission_time / (-log(target))^(1/beta)

  if (!is.null(n) & is.null(test_time)) {
    # Solve for required test time
    T_req <- eta0 * ((-log(1 - conf_level)) / n)^(1/beta)
    result <- list(Distribution = ifelse(beta==1,"Exponential","Weibull"),
                Beta = beta,
                Target_Reliability = target,
                Mission_Time = mission_time,
                Required_Test_Time = T_req,
                Input_Sample_Size = n)

  } else if (is.null(n) & !is.null(test_time)) {
    # Solve for required sample size
    n_req <- ceiling((-log(1 - conf_level)) / ((test_time/eta0)^beta))
    result <- list(Distribution = ifelse(beta==1,"Exponential","Weibull"),
                Beta = beta,
                Target_Reliability = target,
                Mission_Time = mission_time,
                Required_Sample_Size = n_req,
                Input_Test_Time = test_time)

  } else {
    stop("Please provide exactly one of n (sample size) or test_time (test time).")
  }

  class(result) <- "rdt"
  return(result)

}

#' Print method for rdt objects
#'
#' This function provides a formatted print method for objects of class `rdt`.
#' @param x An object of class `rdt`.
#' @param ... Additional arguments (not used).
#' @examples
#' plan <- rdt(target=0.9, mission_time=1000, conf_level=0.9, beta=1, n=10)
#' print(plan)
#' @return Invisibly returns the input object.
#' @export
print.rdt <- function(x, ...) {
  cat("Reliability Demonstration Test (RDT) Plan\n")
  cat("-----------------------------------------\n")
  cat("Distribution: ", x$Distribution, "\n")
  cat("Weibull Shape Parameter (Beta): ", x$Beta, "\n")
  cat("Target Reliability: ", x$Target_Reliability, "\n")
  cat("Mission Time: ", x$Mission_Time, "\n")

  if (!is.null(x$Required_Test_Time)) {
    cat("Input Sample Size (n): ", x$Input_Sample_Size, "\n")
    cat("Required Test Time (T): ", round(x$Required_Test_Time, 2), "\n")
  } else if (!is.null(x$Required_Sample_Size)) {
    cat("Input Test Time (T): ", x$Input_Test_Time, "\n")
    cat("Required Sample Size (n): ", x$Required_Sample_Size, "\n")
  }
}



