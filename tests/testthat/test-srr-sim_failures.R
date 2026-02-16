test_that("sim_failures returns correct output structure", {
  set.seed(1)
  runtimes <- c(100, 500, 200, 800, 300)
  result <- sim_failures(2, runtimes)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("index", "runtime"))
  expect_equal(nrow(result), 2)
  expect_type(result$index, "integer")
  expect_type(result$runtime, "double")
  # sorted ascending by runtime
  expect_true(all(diff(result$runtime) >= 0))
})

test_that("sim_failures PPS bias: higher-runtime units selected more often", {
  set.seed(99)
  runtimes <- c(10, 1000)
  N <- 10000
  draws <- replicate(N, {
    sim_failures(1, runtimes, replace = FALSE)$index
  })
  # unit 2 has 100x the runtime of unit 1, so it should be chosen ~99% of the time
  prop_unit2 <- mean(draws == 2)
  expect_gt(prop_unit2, 0.95)
})

test_that("sim_failures errors: n is not numeric or not scalar", {
  expect_error(sim_failures("2", c(1, 2, 3)), "'n' must be a single numeric value.")
  expect_error(sim_failures(c(1, 2), c(1, 2, 3)), "'n' must be a single numeric value.")
})

test_that("sim_failures errors: n is NA, NaN, or Inf", {
  expect_error(sim_failures(NA_real_, c(1, 2, 3)), "'n' must be a finite positive integer.")
  expect_error(sim_failures(NaN, c(1, 2, 3)), "'n' must be a finite positive integer.")
  expect_error(sim_failures(Inf, c(1, 2, 3)), "'n' must be a finite positive integer.")
})

test_that("sim_failures errors: n is not a positive whole number", {
  expect_error(sim_failures(0, c(1, 2, 3)), "'n' must be a positive integer.")
  expect_error(sim_failures(-1, c(1, 2, 3)), "'n' must be a positive integer.")
  expect_error(sim_failures(1.5, c(1, 2, 3)), "'n' must be a positive integer.")
})

test_that("sim_failures errors: runtimes is not a numeric vector", {
  expect_error(sim_failures(1, c("a", "b")), "'runtimes' must be a numeric vector.")
  expect_error(sim_failures(1, TRUE), "'runtimes' must be a numeric vector.")
})

test_that("sim_failures errors: runtimes is empty", {
  expect_error(sim_failures(1, numeric(0)), "'runtimes' cannot be empty.")
})

test_that("sim_failures errors: runtimes contains NA or NaN", {
  expect_error(sim_failures(1, c(1, NA, 3)), "'runtimes' contains missing \\(NA\\) or NaN values.")
  expect_error(sim_failures(1, c(1, NaN, 3)), "'runtimes' contains missing \\(NA\\) or NaN values.")
})

test_that("sim_failures errors: runtimes contains non-finite or <= 0 values", {
  expect_error(sim_failures(1, c(1, Inf, 3)), "All values in 'runtimes' must be finite and > 0.")
  expect_error(sim_failures(1, c(0, 1, 2)), "All values in 'runtimes' must be finite and > 0.")
  expect_error(sim_failures(1, c(-1, 1, 2)), "All values in 'runtimes' must be finite and > 0.")
})

test_that("sim_failures errors: replace is not a logical scalar", {
  expect_error(sim_failures(1, c(1, 2, 3), replace = 1), "'replace' must be a single logical value \\(TRUE or FALSE\\).")
  expect_error(sim_failures(1, c(1, 2, 3), replace = NA), "'replace' must be a single logical value \\(TRUE or FALSE\\).")
  expect_error(sim_failures(1, c(1, 2, 3), replace = c(TRUE, FALSE)), "'replace' must be a single logical value \\(TRUE or FALSE\\).")
})

test_that("sim_failures errors: n > length(runtimes) without replacement", {
  expect_error(
    sim_failures(4, c(100, 200, 300), replace = FALSE),
    "'n' cannot exceed the number of units in 'runtimes' when replace = FALSE."
  )
  # Same call with replace = TRUE should succeed
  set.seed(1)
  result <- sim_failures(4, c(100, 200, 300), replace = TRUE)
  expect_equal(nrow(result), 4)
})

test_that("sim_failures is reproducible with the same seed", {
  runtimes <- c(100, 500, 200, 800, 300)

  set.seed(42)
  r1 <- sim_failures(3, runtimes)

  set.seed(42)
  r2 <- sim_failures(3, runtimes)

  expect_equal(r1, r2)
})

test_that("sim_failures edge case: n = 1 returns a 1-row data frame", {
  set.seed(7)
  runtimes <- c(100, 500, 200)
  result <- sim_failures(1, runtimes)
  expect_equal(nrow(result), 1)
  expect_named(result, c("index", "runtime"))
})

test_that("sim_failures edge case: n = length(runtimes) returns all units without replacement", {
  set.seed(7)
  runtimes <- c(100, 500, 200, 800, 300)
  result <- sim_failures(length(runtimes), runtimes, replace = FALSE)
  expect_equal(nrow(result), length(runtimes))
  expect_equal(sort(result$index), seq_along(runtimes))
})
