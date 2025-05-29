test_that("plot_rga() works with default settings", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_silent(plot_rga(result))  # No output or error
})

test_that("plot_rga() handles log scale plotting", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_silent(plot_rga(result, log = TRUE))
})

test_that("plot_rga() can suppress legend and confidence bounds", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_silent(plot_rga(result, conf_bounds = FALSE, legend = FALSE))
})

test_that("plot_rga() accepts custom legend position", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_silent(plot_rga(result, legend_pos = "topright"))
})

test_that("plot_rga() throws error for invalid rga_obj", {
  expect_error(plot_rga(list(fake = TRUE)), "Input must be an object of class 'rga'")
})

test_that("plot_rga() throws error for malformed model data", {
  broken_obj <- structure(list(model = list(model = list(foo = 1))), class = "rga")
  expect_error(plot_rga(broken_obj), "appears malformed or missing model data")
})

test_that("plot_rga() throws error for non-logical inputs", {
  times <- c(100, 200, 300, 400, 500)
  failures <- c(1, 2, 1, 3, 2)
  result <- rga(times, failures)

  expect_error(plot_rga(result, conf_bounds = "yes"), "must be logical")
  expect_error(plot_rga(result, legend = 1), "must be logical")
  expect_error(plot_rga(result, log = "false"), "must be logical")
})
