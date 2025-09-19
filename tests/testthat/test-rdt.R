test_that("rdt errors on invalid inputs", {
  expect_error(rdt(target = -0.1, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10),
               "Target reliability must be between 0 and 1")
  expect_error(rdt(target = 0.9, mission_time = 0, conf_level = 0.9, beta = 1, n = 10),
               "Mission time must be greater than 0")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 1.1, beta = 1, n = 10),
               "Confidence level must be between 0 and 1")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 0, n = 10),
               "Shape parameter beta must be greater than 0")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10, test_time = 2000),
               "Please provide only one of n")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1),
               "Please provide one of n")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = -5),
               "Sample size n must be a positive integer")
  expect_error(rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 3.5),
               "Sample size n must be a positive integer")
})

test_that("rdt calculates required test time correctly", {
  plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
  expect_s3_class(plan, "rdt")
  expect_equal(plan$Distribution, "Exponential")
  expect_equal(plan$Beta, 1)
  expect_equal(plan$Target_Reliability, 0.9)
  expect_equal(plan$Mission_Time, 1000)
  expect_equal(plan$Input_Sample_Size, 10)
  # check approximate value for required test time
  expect_equal(round(plan$Required_Test_Time, 2), 2185.43, tolerance = 1e-2)
})

test_that("rdt calculates required sample size correctly", {
  plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, test_time = 2000)
  expect_s3_class(plan, "rdt")
  expect_equal(plan$Distribution, "Exponential")
  expect_equal(plan$Beta, 1)
  expect_equal(plan$Target_Reliability, 0.9)
  expect_equal(plan$Mission_Time, 1000)
  expect_equal(plan$Input_Test_Time, 2000)
  expect_equal(plan$Required_Sample_Size, 11) # computed from formula
})

test_that("rdt works with Weibull distribution (beta != 1)", {
  plan <- rdt(target = 0.95, mission_time = 500, conf_level = 0.9, beta = 2, n = 5)
  expect_equal(plan$Distribution, "Weibull")
  expect_equal(plan$Beta, 2)
  expect_equal(plan$Target_Reliability, 0.95)
  expect_equal(plan$Mission_Time, 500)
  expect_equal(plan$Input_Sample_Size, 5)
  expect_true(plan$Required_Test_Time > 0)
})

test_that("print.rdt outputs formatted text", {
  plan <- rdt(target = 0.9, mission_time = 1000, conf_level = 0.9, beta = 1, n = 10)
  expect_output(print(plan), "Reliability Demonstration Test")
  expect_output(print(plan), "Distribution:  Exponential")
  expect_output(print(plan), "Input Sample Size")
  expect_output(print(plan), "Required Test Time")
})
