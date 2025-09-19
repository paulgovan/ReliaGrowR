# helper to create a fake minimal rga object for testing
fake_rga <- function(beta = 1.5, lambda = 0.002, times = c(5, 10, 20, 30)) {
  obj <- list(
    betas = beta,
    lambdas = lambda,
    model = list(model = list(log_times = log(times)))
  )
  class(obj) <- "rga"
  obj
}

test_that("qqplot.rga errors on invalid input", {
  expect_error(qqplot.rga(list()), "Input must be of class 'rga'")
})

test_that("ppplot.rga errors on invalid input", {
  expect_error(ppplot.rga(list()), "Input must be of class 'rga'")
})

test_that("qqplot.rga runs silently and produces a plot", {
  fit <- fake_rga()
  expect_silent(qqplot.rga(fit))
})

test_that("ppplot.rga runs silently and produces a plot", {
  fit <- fake_rga()
  expect_silent(ppplot.rga(fit))
})

test_that("qqplot.rga extracts parameters correctly (vector case)", {
  fit <- fake_rga(beta = 2, lambda = 0.01)
  expect_silent(qqplot.rga(fit))
  expect_equal(fit$betas, 2)
  expect_equal(fit$lambdas, 0.01)
})

test_that("ppplot.rga extracts parameters correctly (matrix/list case)", {
  # Fake list/matrix structure like real RGA fits
  fit <- list(
    betas = list(log_times = data.frame(`Est.` = c(1.2, 2.3))),
    lambdas = matrix(c(0.005, 0.01), ncol = 1,
                     dimnames = list(NULL, "Est.")),
    model = list(model = list(log_times = log(c(3, 6, 12, 18))))
  )
  class(fit) <- "rga"

  expect_silent(ppplot.rga(fit))
  expect_true(is.list(fit$betas))
  expect_true(is.matrix(fit$lambdas))
})

test_that("qqplot.rga plot matches expected", {
  fit <- fake_rga()
  vdiffr::expect_doppelganger("qqplot rga", function() qqplot.rga(fit))
})

test_that("ppplot.rga plot matches expected", {
  fit <- fake_rga()
  vdiffr::expect_doppelganger("ppplot rga", function() ppplot.rga(fit))
})


