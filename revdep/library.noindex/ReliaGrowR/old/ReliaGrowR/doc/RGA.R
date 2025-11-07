## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ReliaGrowR)

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
result <- rga(times, failures)
plot(result, main = "Crow-AMSAA Model", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(25, 55, 97, 146, 201, 268, 341, 423, 513, 609, 710, 820, 940, 1072, 1217)
failures <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)
breaks <- 500

## -----------------------------------------------------------------------------
result <- rga(times, failures, model_type = "Piecewise NHPP", breaks = breaks)
plot(result, main = "Piecewise NHPP Model", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(25, 55, 97, 146, 201, 268, 341, 423, 513, 609, 710, 820, 940, 1072, 1217)
failures <- c(1, 1, 2, 4, 4, 1, 1, 2, 1, 4, 1, 1, 3, 3, 4)

## -----------------------------------------------------------------------------
result <- rga(times, failures, model_type = "Piecewise NHPP")
plot(result, main = "Piecewise NHPP with Change Point Detection", xlab = "Cumulative Time", ylab = "Cumulative Failures")

## -----------------------------------------------------------------------------
times <- c(100, 200, 300, 400, 500)
failures <- c(1, 2, 1, 3, 2)

## -----------------------------------------------------------------------------
fit <- duane(times, failures)
plot(fit, main = "Duane Plot", xlab = "Cumulative Time", ylab = "Cumulative MTBF", col = "blue")

