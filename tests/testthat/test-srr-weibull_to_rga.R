test_that("weibull_to_rga() input validation errors", {
  # failures
  expect_error(
    weibull_to_rga("a"),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, -2, 3)),
    "`failures` must be a numeric vector with positive values."
  )
  expect_error(
    weibull_to_rga(c(1, NA, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, NaN, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, Inf, 3)),
    "`failures` contains missing \\(NA\\), NaN, or infinite values."
  )

  # suspensions
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = "a"),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, -2)),
    "`suspensions` must be a numeric vector with positive values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, NA)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, NaN)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), suspensions = c(1, Inf)),
    "`suspensions` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval bounds missing
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2)),
    "Both `interval_starts` and `interval_ends` must be provided together."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_ends = c(1, 2)),
    "Both `interval_starts` and `interval_ends` must be provided together."
  )

  # interval bounds type
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = "a", interval_ends = 2),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = 1, interval_ends = "b"),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval bounds length mismatch
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = 3),
    "`interval_starts` and `interval_ends` must have the same length."
  )

  # interval bounds positivity
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, -2), interval_ends = c(2, 3)),
    "Interval bounds must be positive."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(-2, 3)),
    "Interval bounds must be positive."
  )

  # interval_starts bad values
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, NA), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, NaN), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, Inf), interval_ends = c(2, 3)),
    "`interval_starts` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval_ends bad values
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(NA, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(NaN, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(1, 2), interval_ends = c(Inf, 3)),
    "`interval_ends` contains missing \\(NA\\), NaN, or infinite values."
  )

  # interval_starts >= interval_ends
  expect_error(
    weibull_to_rga(c(1, 2), interval_starts = c(5, 10), interval_ends = c(5, 8)),
    "Each interval start must be strictly less than its corresponding end."
  )
})
