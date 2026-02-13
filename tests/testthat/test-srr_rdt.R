#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.4} Unit tests include correctness tests to test that statistical
#' algorithms produce expected results to some fixed test data sets.
#' @srrstats {G5.5} Correctness tests are run with a fixed random seed.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.9} Unit tests include noise susceptibility tests for expected stochastic behavior.
#' @srrstats {G5.9a} Unit tests check that adding trivial noise to data does not meaningfully change results.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

test_that("rdt() input validation errors", {
  # target
  expect_error(
    rdt("a", 100, 0.9, n = 10),
    "'target' must be a single finite numeric value."
  )
  expect_error(
    rdt(c(0.9, 0.8), 100, 0.9, n = 10),
    "'target' must be a single finite numeric value."
  )
  expect_error(
    rdt(Inf, 100, 0.9, n = 10),
    "'target' must be a single finite numeric value."
  )
  # expect_error(rdt(0, 100, 0.9, n = 10),
  #              "'target' must be between 0 and 1 (exclusive).")
  # expect_error(rdt(1, 100, 0.9, n = 10),
  #              "'target' must be between 0 and 1 (exclusive).")

  # mission_time
  expect_error(
    rdt(0.9, "a", 0.9, n = 10),
    "'mission_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, c(100, 200), 0.9, n = 10),
    "'mission_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, Inf, 0.9, n = 10),
    "'mission_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 0, 0.9, n = 10),
    "'mission_time' must be greater than 0."
  )

  # conf_level
  expect_error(
    rdt(0.9, 100, "a", n = 10),
    "'conf_level' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, c(0.9, 0.8), n = 10),
    "'conf_level' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, Inf, n = 10),
    "'conf_level' must be a single finite numeric value."
  )
  # expect_error(rdt(0.9, 100, 0, n = 10),
  #              "'conf_level' must be between 0 and 1 (exclusive).")
  # expect_error(rdt(0.9, 100, 1, n = 10),
  #              "'conf_level' must be between 0 and 1 (exclusive).")

  # beta
  expect_error(
    rdt(0.9, 100, 0.9, beta = "a", n = 10),
    "'beta' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, beta = c(1, 2), n = 10),
    "'beta' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, beta = Inf, n = 10),
    "'beta' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, beta = 0, n = 10),
    "'beta' must be greater than 0."
  )

  # n vs test_time mutual exclusivity
  expect_error(
    rdt(0.9, 100, 0.9, n = 10, test_time = 200),
    "Provide only one of 'n' \\(sample size\\) or 'test_time', not both."
  )
  expect_error(
    rdt(0.9, 100, 0.9),
    "Provide exactly one of 'n' \\(sample size\\) or 'test_time'."
  )

  # n validation
  expect_error(
    rdt(0.9, 100, 0.9, n = "a"),
    "'n' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, n = c(1, 2)),
    "'n' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, n = Inf),
    "'n' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, n = 0),
    "'n' must be a positive integer."
  )
  expect_error(
    rdt(0.9, 100, 0.9, n = 3.5),
    "'n' must be a positive integer."
  )

  # test_time validation
  expect_error(
    rdt(0.9, 100, 0.9, test_time = "a"),
    "'test_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, test_time = c(1, 2)),
    "'test_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, test_time = Inf),
    "'test_time' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, test_time = 0),
    "'test_time' must be greater than 0."
  )
})

test_that("print.rdt() input validation errors", {
  expect_error(
    print.rdt(list()),
    "'x' must be an object of class 'rdt'."
  )
})

test_that("RDT with sample size input is stable to trivial noise", {
  set.seed(123)

  # Base parameters
  target <- 0.9
  mission_time <- 1000
  conf_level <- 0.9
  beta <- 1
  n <- 10

  base_result <- rdt(target, mission_time, conf_level, beta, n = n)

  # Apply very small noise to continuous parameters
  noise <- function(x) x + rnorm(1, 0, 1e-8)
  noisy_result <- rdt(noise(target), noise(mission_time), noise(conf_level),
    noise(beta),
    n = n
  )

  # Sample size input stays exact
  expect_equal(base_result$Input_Sample_Size, noisy_result$Input_Sample_Size)

  # Required test time should be nearly identical
  expect_equal(base_result$Required_Test_Time,
    noisy_result$Required_Test_Time,
    tolerance = 1e-6
  )
})

test_that("RDT with test time input is stable to trivial noise", {
  set.seed(456)

  # Base parameters
  target <- 0.9
  mission_time <- 1000
  conf_level <- 0.9
  beta <- 1
  test_time <- 2000

  base_result <- rdt(target, mission_time, conf_level, beta, test_time = test_time)

  # Apply very small noise
  noise <- function(x) x + rnorm(1, 0, 1e-8)
  noisy_result <- rdt(noise(target), noise(mission_time), noise(conf_level),
    noise(beta),
    test_time = noise(test_time)
  )

  # Input test time should remain close
  expect_equal(base_result$Input_Test_Time,
    noisy_result$Input_Test_Time,
    tolerance = 1e-6
  )

  # Required sample size is integer-valued; expect identical result
  expect_equal(base_result$Required_Sample_Size, noisy_result$Required_Sample_Size)
})

test_that("print.rdt works for required test time", {
  plan <- rdt(
    target = 0.9, mission_time = 1000,
    conf_level = 0.9, beta = 1, n = 10
  )

  # Ensure output contains expected lines
  expect_output(print(plan), "Reliability Demonstration Test \\(RDT\\) Plan")
  expect_output(print(plan), "Distribution:")
  expect_output(print(plan), "Weibull Shape Parameter")
  expect_output(print(plan), "Target Reliability")
  expect_output(print(plan), "Mission Time")
  expect_output(print(plan), "Input Sample Size")
  expect_output(print(plan), "Required Test Time")

  # Invisibly returns the input object
  expect_invisible(print(plan))
})

test_that("print.rdt works for required sample size", {
  plan <- rdt(
    target = 0.9, mission_time = 1000,
    conf_level = 0.9, beta = 1, test_time = 2000
  )

  expect_output(print(plan), "Reliability Demonstration Test \\(RDT\\) Plan")
  expect_output(print(plan), "Distribution:")
  expect_output(print(plan), "Weibull Shape Parameter")
  expect_output(print(plan), "Target Reliability")
  expect_output(print(plan), "Mission Time")
  expect_output(print(plan), "Input Test Time")
  expect_output(print(plan), "Required Sample Size")

  expect_invisible(print(plan))
})

test_that("print.rdt errors on wrong input", {
  expect_error(print.rdt(123), "'x' must be an object of class 'rdt'")
  expect_error(print.rdt(list()), "'x' must be an object of class 'rdt'")
})

test_that("rdt() validates f parameter", {
  expect_error(
    rdt(0.9, 100, 0.9, f = "a", n = 10),
    "'f' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, f = c(0, 1), n = 10),
    "'f' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, f = Inf, n = 10),
    "'f' must be a single finite numeric value."
  )
  expect_error(
    rdt(0.9, 100, 0.9, f = -1, n = 10),
    "'f' must be a non-negative integer."
  )
  expect_error(
    rdt(0.9, 100, 0.9, f = 1.5, n = 10),
    "'f' must be a non-negative integer."
  )
})

test_that("rdt() f=0 is backward compatible with default", {
  plan_default <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
  plan_f0 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 0, n = 10)
  expect_equal(plan_default$Required_Test_Time, plan_f0$Required_Test_Time)
  expect_equal(plan_f0$Allowed_Failures, 0)

  plan_default2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, test_time = 2000)
  plan_f0_2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 0, test_time = 2000)
  expect_equal(plan_default2$Required_Sample_Size, plan_f0_2$Required_Sample_Size)
})

test_that("rdt() f>0 correctness against chi-squared formula", {
  target <- 0.9
  mt <- 1000
  cl <- 0.9
  f <- 2
  eta0 <- mt / (-log(target))

  # Solve for test time
  expected_T <- eta0 * qchisq(cl, df = 2 * (f + 1)) / (2 * 10)
  plan <- rdt(target = target, mission_time = mt, conf_level = cl, beta = 1, f = f, n = 10)
  expect_equal(plan$Required_Test_Time, expected_T, tolerance = 1e-10)
  expect_equal(plan$Allowed_Failures, f)

  # Solve for sample size
  expected_n <- ceiling(qchisq(cl, df = 2 * (f + 1)) / (2 * (2000 / eta0)))
  plan2 <- rdt(target = target, mission_time = mt, conf_level = cl, beta = 1, f = f, test_time = 2000)
  expect_equal(plan2$Required_Sample_Size, expected_n)
  expect_equal(plan2$Allowed_Failures, f)
})

test_that("rdt() allowing more failures increases required test time and sample size", {
  # With fixed n, higher f requires longer per-unit test time (more total evidence needed)
  plan0 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 0, n = 10)
  plan2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 2, n = 10)
  expect_gt(plan2$Required_Test_Time, plan0$Required_Test_Time)

  # With fixed test_time, higher f requires more units
  plan0_tt <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 0, test_time = 2000)
  plan2_tt <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 2, test_time = 2000)
  expect_gt(plan2_tt$Required_Sample_Size, plan0_tt$Required_Sample_Size)
})

test_that("RDT with f>0 and sample size input is stable to trivial noise", {
  set.seed(789)
  target <- 0.9
  mission_time <- 1000
  conf_level <- 0.9
  beta <- 1
  f <- 2
  n <- 10

  base_result <- rdt(target, mission_time, conf_level, beta, f = f, n = n)

  noise <- function(x) x + rnorm(1, 0, 1e-8)
  noisy_result <- rdt(noise(target), noise(mission_time), noise(conf_level), noise(beta),
    f = f, n = n
  )

  expect_equal(base_result$Input_Sample_Size, noisy_result$Input_Sample_Size)
  expect_equal(base_result$Required_Test_Time, noisy_result$Required_Test_Time, tolerance = 1e-6)
})

test_that("print.rdt shows Allowed Failures line", {
  plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 2, n = 10)
  expect_output(print(plan), "Allowed Failures")

  plan2 <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, f = 2, test_time = 2000)
  expect_output(print(plan2), "Allowed Failures")
})
