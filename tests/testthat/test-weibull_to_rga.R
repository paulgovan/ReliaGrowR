test_that("weibull_to_rga returns correct output for valid inputs", {
  failures <- c(100, 200, 200, 400)
  suspensions <- c(250, 350, 450)
  result <- weibull_to_rga(failures, suspensions)

  # Check if the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check if the column names are correct
  expect_named(result, c("CumulativeTime", "Failures"))

  # Check if the values are as expected
  expect_equal(nrow(result), 3) # Only failure times with counts > 0
  # expect_equal(result$CumulativeTime, cumsum(failures))
  expect_equal(result$Failures, c(1, 2, 1)) # Aggregated failure counts
})

test_that("weibull_to_rga handles empty failures gracefully", {
  failures <- ""
  suspensions <- c(100, 200)

  expect_error(
    weibull_to_rga(failures, suspensions),
    "Error: `failures` must be a numeric vector with positive values."
  )
})

test_that("weibull_to_rga handles invalid inputs for failures", {
  # Negative values in failures
  expect_error(
    weibull_to_rga(c(-100, 200), NULL),
    "Error: `failures` must be a numeric vector with positive values."
  )

  # Non-numeric values in failures
  expect_error(
    weibull_to_rga(c("a", "b"), NULL),
    "Error: `failures` must be a numeric vector with positive values."
  )
})

test_that("weibull_to_rga handles invalid suspensions correctly", {
  failures <- c(100, 200)

  # Negative values in suspensions
  expect_error(
    weibull_to_rga(failures, c(-100, -200)),
    "Error: suspensions must be a numeric vector with positive values."
  )

  # Non-numeric values in suspensions
  expect_error(
    weibull_to_rga(failures, c("a", "b")),
    "Error: suspensions must be a numeric vector with positive values."
  )
})

test_that("weibull_to_rga works with NULL suspensions", {
  failures <- c(100, 200, 400)
  result <- weibull_to_rga(failures, NULL)

  # Check if the result is as expected
  expect_s3_class(result, "data.frame")
  expect_equal(result$CumulativeTime, cumsum(failures))
  expect_equal(result$Failures, c(1, 1, 1)) # Each failure is counted
})

test_that("weibull_to_rga handles duplicate failure times correctly", {
  failures <- c(100, 200, 200, 400)
  suspensions <- c(250)

  result <- weibull_to_rga(failures, suspensions)

  # Check aggregated failure counts
  expect_equal(result$Failures, c(1, 2, 1)) # Aggregated counts
})
