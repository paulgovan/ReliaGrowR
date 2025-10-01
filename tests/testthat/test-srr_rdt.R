
test_that("rdt() input validation errors", {
  # target
  expect_error(rdt("a", 100, 0.9, n = 10),
               "'target' must be a single finite numeric value.")
  expect_error(rdt(c(0.9, 0.8), 100, 0.9, n = 10),
               "'target' must be a single finite numeric value.")
  expect_error(rdt(Inf, 100, 0.9, n = 10),
               "'target' must be a single finite numeric value.")
  # expect_error(rdt(0, 100, 0.9, n = 10),
  #              "'target' must be between 0 and 1 (exclusive).")
  # expect_error(rdt(1, 100, 0.9, n = 10),
  #              "'target' must be between 0 and 1 (exclusive).")

  # mission_time
  expect_error(rdt(0.9, "a", 0.9, n = 10),
               "'mission_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, c(100, 200), 0.9, n = 10),
               "'mission_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, Inf, 0.9, n = 10),
               "'mission_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, 0, 0.9, n = 10),
               "'mission_time' must be greater than 0.")

  # conf_level
  expect_error(rdt(0.9, 100, "a", n = 10),
               "'conf_level' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, c(0.9, 0.8), n = 10),
               "'conf_level' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, Inf, n = 10),
               "'conf_level' must be a single finite numeric value.")
  # expect_error(rdt(0.9, 100, 0, n = 10),
  #              "'conf_level' must be between 0 and 1 (exclusive).")
  # expect_error(rdt(0.9, 100, 1, n = 10),
  #              "'conf_level' must be between 0 and 1 (exclusive).")

  # beta
  expect_error(rdt(0.9, 100, 0.9, beta = "a", n = 10),
               "'beta' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, beta = c(1, 2), n = 10),
               "'beta' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, beta = Inf, n = 10),
               "'beta' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, beta = 0, n = 10),
               "'beta' must be greater than 0.")

  # n vs test_time mutual exclusivity
  expect_error(rdt(0.9, 100, 0.9, n = 10, test_time = 200),
               "Provide only one of 'n' \\(sample size\\) or 'test_time', not both.")
  expect_error(rdt(0.9, 100, 0.9),
               "Provide exactly one of 'n' \\(sample size\\) or 'test_time'.")

  # n validation
  expect_error(rdt(0.9, 100, 0.9, n = "a"),
               "'n' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, n = c(1, 2)),
               "'n' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, n = Inf),
               "'n' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, n = 0),
               "'n' must be a positive integer.")
  expect_error(rdt(0.9, 100, 0.9, n = 3.5),
               "'n' must be a positive integer.")

  # test_time validation
  expect_error(rdt(0.9, 100, 0.9, test_time = "a"),
               "'test_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, test_time = c(1, 2)),
               "'test_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, test_time = Inf),
               "'test_time' must be a single finite numeric value.")
  expect_error(rdt(0.9, 100, 0.9, test_time = 0),
               "'test_time' must be greater than 0.")
})

test_that("print.rdt() input validation errors", {
  expect_error(print.rdt(list()),
               "'x' must be an object of class 'rdt'.")
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
                      noise(beta), n = n)

  # Sample size input stays exact
  expect_equal(base_result$Input_Sample_Size, noisy_result$Input_Sample_Size)

  # Required test time should be nearly identical
  expect_equal(base_result$Required_Test_Time,
               noisy_result$Required_Test_Time,
               tolerance = 1e-6)
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
                      noise(beta), test_time = noise(test_time))

  # Input test time should remain close
  expect_equal(base_result$Input_Test_Time,
               noisy_result$Input_Test_Time,
               tolerance = 1e-6)

  # Required sample size is integer-valued; expect identical result
  expect_equal(base_result$Required_Sample_Size, noisy_result$Required_Sample_Size)
})
