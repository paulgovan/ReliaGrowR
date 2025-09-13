test_that("weibull_to_rga rejects invalid failures input", {
  expect_error(weibull_to_rga(failures = "a"), "numeric")
  expect_error(weibull_to_rga(failures = c(-1, 2, 3)), "positive")
})

test_that("weibull_to_rga rejects invalid suspensions input", {
  expect_error(weibull_to_rga(failures = 1, suspensions = "a"), "numeric")
  expect_error(weibull_to_rga(failures = 1, suspensions = c(-5, 10)), "positive")
})

test_that("weibull_to_rga rejects invalid interval input", {
  # Only one provided
  expect_error(weibull_to_rga(failures = 1, interval_starts = c(1, 2)), "must be provided together")
  expect_error(weibull_to_rga(failures = 1, interval_ends = c(1, 2)), "must be provided together")

  # Non-numeric
  expect_error(weibull_to_rga(failures = 1, interval_starts = c("a"), interval_ends = c(2)), "numeric")

  # Different lengths
  expect_error(weibull_to_rga(failures = 1, interval_starts = c(1, 2), interval_ends = c(3)), "same length")

  # Non-positive
  expect_error(weibull_to_rga(failures = 1, interval_starts = c(-1), interval_ends = c(3)), "positive")

  # Start >= End
  expect_error(weibull_to_rga(failures = 1, interval_starts = c(5), interval_ends = c(5)), "strictly less")
})

test_that("weibull_to_rga handles only failures", {
  failures <- c(100, 200, 200, 400)
  result <- weibull_to_rga(failures = failures)

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("CumulativeTime", "Failures"))
  expect_true(all(result$Failures > 0))
})

test_that("weibull_to_rga handles failures + suspensions", {
  failures <- c(100, 200, 200, 400)
  suspensions <- c(250, 350, 450)
  result <- weibull_to_rga(failures = failures, suspensions = suspensions)

  expect_true(all(result$Failures > 0))
  expect_gt(nrow(result), 0)
  expect_equal(colnames(result), c("CumulativeTime", "Failures"))
})

test_that("weibull_to_rga handles interval-censored data", {
  failures <- c(100, 200)
  interval_starts <- c(150, 300)
  interval_ends <- c(180, 320)

  result <- weibull_to_rga(failures = failures,
                           interval_starts = interval_starts,
                           interval_ends = interval_ends)

  # Midpoints should appear in CumulativeTime calculation
  midpoints <- (interval_starts + interval_ends) / 2
  expect_true(any(cumsum(sort(c(failures, midpoints))) %in% result$CumulativeTime))
  expect_true(all(result$Failures > 0))
})

test_that("weibull_to_rga produces deterministic output", {
  failures <- c(100, 200, 400)
  suspensions <- c(250)
  interval_starts <- c(150)
  interval_ends <- c(180)

  result1 <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)
  result2 <- weibull_to_rga(failures, suspensions, interval_starts, interval_ends)

  expect_equal(result1, result2)
})
