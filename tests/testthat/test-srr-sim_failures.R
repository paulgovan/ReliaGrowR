test_that("sim_failures returns correct output structure", {
  set.seed(1)
  runtimes <- c(100, 500, 200, 800, 300)
  result <- sim_failures(2, runtimes)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("index", "runtime", "type"))
  expect_equal(nrow(result), 5)
  expect_type(result$index, "integer")
  expect_type(result$runtime, "double")
  expect_type(result$type, "character")
  expect_true(all(result$type %in% c("Failure", "Suspension")))
  # sorted ascending by runtime
  expect_true(all(diff(result$runtime) >= 0))
})

test_that("sim_failures PPS bias: higher-runtime units selected more often", {
  set.seed(99)
  runtimes <- c(10, 1000)
  N <- 10000
  draws <- replicate(N, {
    res <- sim_failures(1, runtimes, replace = FALSE)
    res$index[res$type == "Failure"]
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
  # Same call with replace = TRUE should succeed (returns full fleet of 3 rows)
  set.seed(1)
  result <- sim_failures(4, c(100, 200, 300), replace = TRUE)
  expect_equal(nrow(result), 3)
})

test_that("sim_failures is reproducible with the same seed", {
  runtimes <- c(100, 500, 200, 800, 300)

  set.seed(42)
  r1 <- sim_failures(3, runtimes)

  set.seed(42)
  r2 <- sim_failures(3, runtimes)

  expect_equal(r1, r2)
})

test_that("sim_failures edge case: n = 1 returns full fleet (3 rows)", {
  set.seed(7)
  runtimes <- c(100, 500, 200)
  result <- sim_failures(1, runtimes)
  expect_equal(nrow(result), 3)
  expect_named(result, c("index", "runtime", "type"))
})

test_that("sim_failures edge case: n = length(runtimes) returns all units without replacement", {
  set.seed(7)
  runtimes <- c(100, 500, 200, 800, 300)
  result <- sim_failures(length(runtimes), runtimes, replace = FALSE)
  expect_equal(nrow(result), length(runtimes))
  expect_equal(sort(result$index), seq_along(runtimes))
})

test_that("sim_failures type labels: correct counts of Failure and Suspension", {
  set.seed(42)
  runtimes <- c(100, 500, 200, 800, 300)
  n <- 2
  result <- sim_failures(n, runtimes, replace = FALSE)

  n_failures    <- sum(result$type == "Failure")
  n_suspensions <- sum(result$type == "Suspension")

  # With replace = FALSE and n unique draws, exactly n failures and rest suspensions
  expect_equal(n_failures, n)
  expect_equal(n_suspensions, length(runtimes) - n)
})

test_that("sim_failures window: failure times are in (runtime, runtime + window]", {
  set.seed(42)
  runtimes <- c(100, 500, 200, 800, 300)
  w <- 50
  result <- sim_failures(2, runtimes, window = w)

  failures <- result[result$type == "Failure", ]
  for (i in seq_len(nrow(failures))) {
    orig <- runtimes[failures$index[i]]
    expect_gt(failures$runtime[i], orig)
    expect_lte(failures$runtime[i], orig + w)
  }
})

test_that("sim_failures window: suspension times equal runtime + window exactly", {
  set.seed(42)
  runtimes <- c(100, 500, 200, 800, 300)
  w <- 50
  result <- sim_failures(2, runtimes, window = w)

  suspensions <- result[result$type == "Suspension", ]
  for (i in seq_len(nrow(suspensions))) {
    orig <- runtimes[suspensions$index[i]]
    expect_equal(suspensions$runtime[i], orig + w)
  }
})

test_that("sim_failures window = NULL: runtimes unchanged", {
  set.seed(42)
  runtimes <- c(100, 500, 200, 800, 300)
  result <- sim_failures(2, runtimes, window = NULL)

  for (i in seq_len(nrow(result))) {
    expect_equal(result$runtime[i], runtimes[result$index[i]])
  }
})

test_that("sim_failures window validation errors", {
  runtimes <- c(100, 500, 200)

  expect_error(
    sim_failures(1, runtimes, window = "50"),
    "'window' must be a single positive numeric value."
  )
  expect_error(
    sim_failures(1, runtimes, window = c(10, 20)),
    "'window' must be a single positive numeric value."
  )
  expect_error(
    sim_failures(1, runtimes, window = NA_real_),
    "'window' must be a finite positive numeric value."
  )
  expect_error(
    sim_failures(1, runtimes, window = Inf),
    "'window' must be a finite positive numeric value."
  )
  expect_error(
    sim_failures(1, runtimes, window = 0),
    "'window' must be a positive numeric value."
  )
  expect_error(
    sim_failures(1, runtimes, window = -5),
    "'window' must be a positive numeric value."
  )
})

test_that("sim_failures reproducibility with window", {
  runtimes <- c(100, 500, 200, 800, 300)
  w <- 50

  set.seed(42)
  r1 <- sim_failures(2, runtimes, window = w)

  set.seed(42)
  r2 <- sim_failures(2, runtimes, window = w)

  expect_equal(r1, r2)
})
