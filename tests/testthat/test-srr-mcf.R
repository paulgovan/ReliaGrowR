#' @srrstats {G5.0} Tests use standard recurrent event data.
#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results.
#' @srrstats {G5.2a} Every message produced by stop() is unique.
#' @srrstats {G5.4} Correctness tests with known MCF values.
#' @srrstats {G5.8a} Checks for zero-length data.
#' @srrstats {G5.8b} Checks for unsupported data types.
#' @srrstats {G5.8c} Checks for data with NA fields.

# --- Input validation tests ---

test_that("mcf errors on missing id", {
  expect_error(mcf(time = c(1, 2)), "'id' must be provided.", fixed = TRUE)
})

test_that("mcf errors on missing time", {
  expect_error(mcf(id = c(1, 1)), "'time' must be provided.", fixed = TRUE)
})

test_that("mcf errors on empty id", {
  expect_error(mcf(id = c(), time = c()), "'id' must be provided.", fixed = TRUE)
})

test_that("mcf errors on empty time", {
  expect_error(mcf(id = c(1), time = c()), "'time' must be provided.", fixed = TRUE)
})

test_that("mcf errors on non-numeric time", {
  expect_error(
    mcf(id = c(1, 1), time = c("a", "b")),
    "'time' must be a numeric vector.", fixed = TRUE
  )
})

test_that("mcf errors on NA in time", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, NA)),
    "'time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("mcf errors on NaN in time", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, NaN)),
    "'time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("mcf errors on non-positive time", {
  expect_error(
    mcf(id = c(1, 1), time = c(0, 100)),
    "All values in 'time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("mcf errors on infinite time", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, Inf)),
    "All values in 'time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("mcf errors on mismatched id and time lengths", {
  expect_error(
    mcf(id = c(1, 2, 3), time = c(100, 200)),
    "'id' and 'time' must have the same length.", fixed = TRUE
  )
})

test_that("mcf errors on non-numeric event", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, 200), event = c("a", "b")),
    "'event' must be a numeric vector.", fixed = TRUE
  )
})

test_that("mcf errors on mismatched event length", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, 200), event = c(1)),
    "'event' and 'time' must have the same length.", fixed = TRUE
  )
})

test_that("mcf errors on NA in event", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, 200), event = c(1, NA)),
    "'event' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("mcf errors on invalid event values", {
  expect_error(
    mcf(id = c(1, 1), time = c(100, 200), event = c(1, 2)),
    "'event' must contain only 0 (censored) or 1 (event).", fixed = TRUE
  )
})

test_that("mcf errors on invalid conf_level", {
  expect_error(
    mcf(id = c(1), time = c(100), conf_level = "a"),
    "'conf_level' must be a single numeric value.", fixed = TRUE
  )
  expect_error(
    mcf(id = c(1), time = c(100), conf_level = 0),
    "'conf_level' must be between 0 and 1 (exclusive).", fixed = TRUE
  )
  expect_error(
    mcf(id = c(1), time = c(100), conf_level = 1),
    "'conf_level' must be between 0 and 1 (exclusive).", fixed = TRUE
  )
})

# --- Data frame input tests ---

test_that("mcf works with data frame input", {
  df <- data.frame(id = c(1, 1, 2, 2), time = c(100, 200, 150, 300))
  result <- mcf(data = df)
  expect_s3_class(result, "mcf")
  expect_equal(result$n_systems, 2)
  expect_equal(result$n_events, 4)
})

test_that("mcf data frame errors on missing columns", {
  expect_error(
    mcf(data = data.frame(x = 1:3)),
    "'data' must contain a column named 'id'.", fixed = TRUE
  )
  expect_error(
    mcf(data = data.frame(id = 1:3)),
    "'data' must contain a column named 'time'.", fixed = TRUE
  )
})

test_that("mcf errors on non-data-frame data", {
  expect_error(
    mcf(data = list(id = 1, time = 1)),
    "'data' must be a data frame.", fixed = TRUE
  )
})

test_that("mcf data frame with event column works", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(100, 200, 150, 300),
    event = c(1, 0, 1, 1)
  )
  result <- mcf(data = df)
  expect_s3_class(result, "mcf")
  expect_equal(result$n_events, 3)
})

# --- Correctness tests ---

test_that("mcf computes correct values for simple single-system case", {
  # Single system with events at 100, 200, 300
  id <- c(1, 1, 1)
  time <- c(100, 200, 300)
  result <- mcf(id, time)

  # For single system, MCF = cumulative count: 1, 2, 3
  expect_equal(result$mcf, c(1, 2, 3))
  expect_equal(result$time, c(100, 200, 300))
  expect_equal(result$n_systems, 1)
  expect_equal(result$n_events, 3)
})

test_that("mcf computes correct values for multi-system case", {
  # System 1: events at 100, 300; max_time = 300

# System 2: events at 100, 200; max_time = 200
  # At t=100: d=2, n=2, increment = 1.0
  # At t=200: d=1, n=2, increment = 0.5
  # At t=300: d=1, n=1, increment = 1.0
  id <- c(1, 1, 2, 2)
  time <- c(100, 300, 100, 200)
  result <- mcf(id, time)

  expect_equal(result$time, c(100, 200, 300))
  expect_equal(result$mcf, c(1.0, 1.5, 2.5))
  expect_equal(result$n_systems, 2)
  expect_equal(result$n_events, 4)
})

test_that("mcf with censoring excludes censored observations from events", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 300, 100, 200)
  event <- c(1, 0, 1, 1)  # System 1's second obs is censoring

  result <- mcf(id, time, event)
  # Events at: t=100 (sys 1 + sys 2), t=200 (sys 2)
  # At t=100: d=2, n=2, increment = 1.0
  # At t=200: d=1, n=2, increment = 0.5
  expect_equal(result$time, c(100, 200))
  expect_equal(result$mcf, c(1.0, 1.5))
  expect_equal(result$n_events, 3)
})

test_that("mcf confidence bounds are wider at higher confidence level", {
  id <- c(1, 1, 1, 2, 2, 3, 3, 3)
  time <- c(100, 200, 300, 100, 250, 50, 150, 400)

  result_90 <- mcf(id, time, conf_level = 0.90)
  result_99 <- mcf(id, time, conf_level = 0.99)

  width_90 <- result_90$upper_bounds - result_90$lower_bounds
  width_99 <- result_99$upper_bounds - result_99$lower_bounds

  expect_true(all(width_99 >= width_90))
})

test_that("mcf lower bounds are non-negative", {
  id <- c(1, 1, 2, 2, 3)
  time <- c(100, 200, 150, 300, 50)
  result <- mcf(id, time)
  expect_true(all(result$lower_bounds >= 0))
})

# --- end_time (exposure) tests ---

test_that("mcf with end_time keeps systems in risk set longer", {
  # System 1: event at 100; last event = 100
  # System 2: event at 200; last event = 200
  # Without end_time: at t=200, n=1 (sys1 dropped out at 100)
  # With end_time = 500 for both: at t=200, n=2
  id <- c(1, 2)
  time <- c(100, 200)

  result_no_et <- mcf(id, time)
  result_et <- mcf(id, time, end_time = c("1" = 500, "2" = 500))

  # Without end_time: at t=200, n_j=1 so increment = 1/1 = 1
  # With end_time: at t=200, n_j=2 so increment = 1/2 = 0.5
  # MCF at t=200 should be lower with end_time
  expect_true(result_et$mcf[length(result_et$mcf)] <
                result_no_et$mcf[length(result_no_et$mcf)])
})

test_that("mcf with end_time produces correct values", {
  # System 1: event at 100, observed until 400
  # System 2: event at 100, event at 300, observed until 300
  # At t=100: d=2, n=2, increment = 1.0

  # At t=300: d=1, n=2 (sys1 still at risk until 400), increment = 0.5
  id <- c(1, 2, 2)
  time <- c(100, 100, 300)
  end_time <- c("1" = 400, "2" = 300)

  result <- mcf(id, time, end_time = end_time)

  expect_equal(result$time, c(100, 300))
  expect_equal(result$mcf, c(1.0, 1.5))
})

test_that("mcf with unnamed end_time works", {
  id <- c(1, 2)
  time <- c(100, 200)
  # Unnamed: assumed to be in order of unique(id) = c(1, 2)
  result <- mcf(id, time, end_time = c(500, 500))
  expect_s3_class(result, "mcf")
})

test_that("mcf errors on unnamed end_time with wrong length", {
  id <- c(1, 2)
  time <- c(100, 200)
  expect_error(
    mcf(id, time, end_time = c(500, 500, 500)),
    "Unnamed 'end_time' must have one value per system"
  )
})

test_that("mcf errors on non-numeric end_time", {
  id <- c(1, 2)
  time <- c(100, 200)
  expect_error(
    mcf(id, time, end_time = c("a", "b")),
    "'end_time' must be a numeric vector.", fixed = TRUE
  )
})

test_that("mcf errors on NA in end_time", {
  id <- c(1, 2)
  time <- c(100, 200)
  expect_error(
    mcf(id, time, end_time = c(500, NA)),
    "'end_time' contains missing (NA) or NaN values.", fixed = TRUE
  )
})

test_that("mcf errors on non-positive end_time", {
  id <- c(1, 2)
  time <- c(100, 200)
  expect_error(
    mcf(id, time, end_time = c(500, 0)),
    "All values in 'end_time' must be finite and > 0.", fixed = TRUE
  )
})

test_that("mcf output includes end_times", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)
  expect_true("end_times" %in% names(result))
})

test_that("mcf end_times from data frame works", {
  df <- data.frame(
    id = c(1, 1, 2, 2),
    time = c(100, 200, 150, 300),
    end_time = c(500, 500, 500, 500)
  )
  result <- mcf(data = df)
  expect_s3_class(result, "mcf")
  # end_time should be 500 for both systems
  expect_equal(unname(result$end_times), c(500, 500))
})

test_that("mcf with exposure end_times interoperates", {
  # Compute exposure, then use its end_times in MCF
  id <- c(1, 1, 1, 2, 2, 2)
  time <- c(100, 300, 500, 80, 300, 600)
  event <- c(1, 1, 0, 1, 1, 0)

  exp_result <- exposure(id, time, event)
  mcf_result <- mcf(id, time, event, end_time = exp_result$end_times)

  expect_s3_class(mcf_result, "mcf")
  # With censoring at 500 and 600, both systems should be at risk
  # longer than if we only considered event times
  expect_true(all(mcf_result$end_times >= 0))
})

# --- Print and plot tests ---

test_that("print.mcf produces output", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)
  expect_output(print(result), "Mean Cumulative Function")
})

test_that("print.mcf returns invisible", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)
  expect_invisible(print(result))
})

test_that("print.mcf errors on wrong class", {
  expect_error(
    print.mcf(list(a = 1)),
    "'x' must be an object of class 'mcf'.", fixed = TRUE
  )
})

test_that("plot.mcf produces a plot without error", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)
  expect_silent(plot(result, main = "Test MCF"))
})

test_that("plot.mcf validates inputs", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)

  expect_error(plot.mcf(list(a = 1)), "'x' must be an object of class 'mcf'.")
  expect_error(plot(result, conf_bounds = "yes"), "'conf_bounds' must be a single logical value.")
  expect_error(plot(result, legend = "yes"), "'legend' must be a single logical value.")
  expect_error(plot(result, legend_pos = 123), "'legend_pos' must be a single character string.")
})

test_that("plot.mcf works without confidence bounds and legend", {
  id <- c(1, 1, 2, 2)
  time <- c(100, 200, 150, 300)
  result <- mcf(id, time)
  expect_silent(plot(result, conf_bounds = FALSE, legend = FALSE))
})
