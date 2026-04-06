#' @srrstats {G5.0} Tests use standard recurrent event data.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by stop() is unique.
#' @srrstats {G5.4} Correctness tests with known exposure values.
#' @srrstats {G5.8a} Checks for zero-length data.
#' @srrstats {G5.8b} Checks for unsupported data types.
#' @srrstats {G5.8c} Checks for data with NA fields.

# --- Input validation tests ---

test_that("exposure errors on missing id", {
  expect_error(exposure(time = c(1, 2)),
               "'id' must be provided to exposure().", fixed = TRUE)
})

test_that("exposure errors on missing time", {
  expect_error(exposure(id = c(1, 1)),
               "'time' must be provided to exposure().", fixed = TRUE)
})

test_that("exposure errors on NULL id", {
  expect_error(exposure(id = NULL, time = c(1, 2)),
               "'id' must be provided to exposure().", fixed = TRUE)
})

test_that("exposure errors on NULL time", {
  expect_error(exposure(id = c(1), time = NULL),
               "'time' must be provided to exposure().", fixed = TRUE)
})

test_that("exposure errors on non-numeric time", {
  expect_error(
    exposure(id = c(1, 1), time = c("a", "b")),
    "'time' must be a numeric vector in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on NA in time", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, NA)),
    "'time' contains missing (NA) or NaN values in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on NaN in time", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, NaN)),
    "'time' contains missing (NA) or NaN values in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on non-positive time", {
  expect_error(
    exposure(id = c(1, 1), time = c(0, 100)),
    "All values in 'time' must be finite and > 0 in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on infinite time", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, Inf)),
    "All values in 'time' must be finite and > 0 in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on mismatched id and time lengths", {
  expect_error(
    exposure(id = c(1, 2, 3), time = c(100, 200)),
    "'id' and 'time' must have the same length in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on non-numeric event", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, 200), event = c("a", "b")),
    "'event' must be a numeric vector in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on mismatched event length", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, 200), event = c(1)),
    "'event' and 'time' must have the same length in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on NA in event", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, 200), event = c(1, NA)),
    "'event' contains missing (NA) or NaN values in exposure().", fixed = TRUE
  )
})

test_that("exposure errors on invalid event values", {
  expect_error(
    exposure(id = c(1, 1), time = c(100, 200), event = c(1, 2)),
    "'event' must contain only 0 (censored) or 1 (event) in exposure().", fixed = TRUE
  )
})

# --- Data frame input tests ---

test_that("exposure works with data frame input", {
  df <- data.frame(id = c(1, 1, 2, 2), time = c(100, 200, 150, 300))
  result <- exposure(data = df)
  expect_s3_class(result, "exposure")
  expect_equal(result$n_systems, 2)
  expect_equal(result$total_events, 4)
})

test_that("exposure data frame errors on missing columns", {
  expect_error(
    exposure(data = data.frame(x = 1:3)),
    "'data' must contain a column named 'id' for exposure().", fixed = TRUE
  )
  expect_error(
    exposure(data = data.frame(id = 1:3)),
    "'data' must contain a column named 'time' for exposure().", fixed = TRUE
  )
})

test_that("exposure errors on non-data-frame data", {
  expect_error(
    exposure(data = list(id = 1, time = 1)),
    "'data' must be a data frame when provided to exposure().", fixed = TRUE
  )
})

test_that("exposure data frame with event column works", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(100, 200, 150, 300),
    event = c(1, 0, 1, 1)
  )
  result <- exposure(data = df)
  expect_s3_class(result, "exposure")
  expect_equal(result$total_events, 3)
})

# --- Correctness tests ---

test_that("exposure computes correct values for single-system case", {
  # Single system, events at 100 and 200
  # max_time = 200
  # At t=100: cum_exposure = min(100, 200) = 100, cum_events = 1
  # At t=200: cum_exposure = min(200, 200) = 200, cum_events = 2
  id <- c(1, 1)
  time <- c(100, 200)
  result <- exposure(id, time)

  expect_equal(result$time, c(100, 200))
  expect_equal(result$n_at_risk, c(1, 1))
  expect_equal(result$cum_exposure, c(100, 200))
  expect_equal(result$cum_events, c(1, 2))
  expect_equal(result$total_exposure, 200)
  expect_equal(result$total_events, 2)
  expect_equal(result$n_systems, 1)
})

test_that("exposure computes correct values for multi-system case", {
  # System 1: events at 100, 300; max_time = 300
  # System 2: events at 200; max_time = 200
  #
  # At t=100: n_at_risk=2, cum_exposure = min(100,300) + min(100,200) = 200
  # At t=200: n_at_risk=2, cum_exposure = min(200,300) + min(200,200) = 400
  # At t=300: n_at_risk=1, cum_exposure = min(300,300) + min(300,200) = 500
  id <- c(1, 1, 2)
  time <- c(100, 300, 200)
  result <- exposure(id, time)

  expect_equal(result$time, c(100, 200, 300))
  expect_equal(result$n_at_risk, c(2, 2, 1))
  expect_equal(result$cum_exposure, c(200, 400, 500))
  expect_equal(result$cum_events, c(1, 2, 3))
  expect_equal(result$total_exposure, 500)
  expect_equal(result$total_events, 3)
})

test_that("exposure event rate is correct", {
  id <- c(1, 1, 2)
  time <- c(100, 300, 200)
  result <- exposure(id, time)

  # Event rate = cum_events / cum_exposure
  expect_equal(result$event_rate, result$cum_events / result$cum_exposure)
})

test_that("exposure with censoring correctly limits observation", {
  # System 1: event at 100, censored at 300
  # System 2: event at 200
  # max_times: sys1=300, sys2=200
  id    <- c(1, 1, 2)
  time  <- c(100, 300, 200)
  event <- c(1, 0, 1)
  result <- exposure(id, time, event)

  # Only event times: 100, 200
  expect_equal(result$time, c(100, 200))
  expect_equal(result$cum_events, c(1, 2))
  expect_equal(result$total_events, 2)
  # Total exposure is still sum of max_times = 300 + 200 = 500
  expect_equal(result$total_exposure, 500)
})

test_that("exposure cum_exposure is monotonically non-decreasing", {
  id <- c(1, 1, 1, 2, 2, 3, 3, 3)
  time <- c(100, 200, 400, 150, 350, 50, 250, 500)
  result <- exposure(id, time)
  expect_true(all(diff(result$cum_exposure) >= 0))
})

test_that("exposure cum_events is monotonically non-decreasing", {
  id <- c(1, 1, 1, 2, 2, 3, 3, 3)
  time <- c(100, 200, 400, 150, 350, 50, 250, 500)
  result <- exposure(id, time)
  expect_true(all(diff(result$cum_events) >= 0))
})

test_that("exposure n_at_risk is monotonically non-increasing", {
  id <- c(1, 1, 2, 2, 3)
  time <- c(100, 200, 100, 300, 100)
  result <- exposure(id, time)
  expect_true(all(diff(result$n_at_risk) <= 0))
})

test_that("exposure handles simultaneous events across systems", {
  # All 3 systems have events at t=100
  id <- c(1, 2, 3)
  time <- c(100, 100, 100)
  result <- exposure(id, time)

  expect_equal(result$time, 100)
  expect_equal(result$n_at_risk, 3)
  expect_equal(result$cum_events, 3)
  expect_equal(result$cum_exposure, 300)
  expect_equal(result$total_exposure, 300)
})

# --- Print and plot tests ---

test_that("print.exposure produces output", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_output(print(result), "Exposure Analysis")
})

test_that("print.exposure returns invisible", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_invisible(print(result))
})

test_that("print.exposure errors on wrong class", {
  expect_error(
    print.exposure(list(a = 1)),
    "'x' must be an object of class 'exposure'.", fixed = TRUE
  )
})

test_that("plot.exposure produces a plot without error", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_silent(plot(result))
})

test_that("plot.exposure validates inputs", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_error(plot.exposure(list(a = 1)),
               "'x' must be an object of class 'exposure'.", fixed = TRUE)
  expect_error(plot(result, legend = "yes"),
               "'legend' must be a single logical value in plot.exposure().", fixed = TRUE)
  expect_error(plot(result, legend_pos = 123),
               "'legend_pos' must be a single character string in plot.exposure().", fixed = TRUE)
})

test_that("plot.exposure works with single panels", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_silent(plot(result, which = "exposure"))
  expect_silent(plot(result, which = "at_risk"))
  expect_silent(plot(result, which = "event_rate"))
})

test_that("plot.exposure works without legend", {
  result <- exposure(c(1, 1, 2, 2), c(100, 200, 150, 300))
  expect_silent(plot(result, legend = FALSE))
})
