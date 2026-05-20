#' @srrstats {G5.2} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.2a} Every message produced by `stop()` is unique.
#' @srrstats {G5.2b} Unit tests demonstrate error messages and compare results with expected values.
#' @srrstats {G5.8} See sub-tags for responses.
#' @srrstats {G5.8a} Unit tests include checks for zero-length data.
#' @srrstats {G5.8b} Unit tests include checks for unsupported data types.
#' @srrstats {G5.8c} Unit tests include checks for data with 'NA' fields.
#' @srrstats {G5.8d} Unit tests include checks for data outside the scope of the algorithm.
#' @srrstats {G5.10} All unit tests run as part of continuous integration.

test_that("qqplot.rga() errors on invalid inputs", {
  # Not an rga object
  expect_error(
    qqplot.rga(list()),
    "'x' must be an object of class 'rga'."
  )

  # Invalid 'main': numeric instead of string
  dummy <- structure(list(), class = "rga")
  expect_error(
    qqplot.rga(dummy, main = 123),
    "'main' must be a single character string."
  )

  # Invalid 'main': character vector of length > 1
  expect_error(
    qqplot.rga(dummy, main = c("a", "b")),
    "'main' must be a single character string."
  )
})

test_that("ppplot.rga() errors on invalid inputs", {
  # Not an rga object
  expect_error(
    ppplot.rga(list()),
    "'x' must be an object of class 'rga'."
  )

  # Invalid 'main': numeric instead of string
  dummy <- structure(list(), class = "rga")
  expect_error(
    ppplot.rga(dummy, main = 999),
    "'main' must be a single character string."
  )

  # Invalid 'main': character vector of length > 1
  expect_error(
    ppplot.rga(dummy, main = c("p", "q")),
    "'main' must be a single character string."
  )
})

# Build a minimal reproducible rga object for testing
make_rga_numeric <- function() {
  times <- c(5, 10, 15, 20, 25)
  failures <- c(1, 2, 1, 3, 2)
  rga(times, failures) # assumes your rga() is available
}

make_rga_matrix <- function() {
  fit <- make_rga_numeric()
  # Overwrite betas and lambdas to matrix/list form to cover branches
  fit$betas <- list(log_times = matrix(
    c(0.9, 0.1),
    nrow = 1, dimnames = list(NULL, c("Est.", "SE"))
  ))
  fit$lambdas <- matrix(
    c(0.002, 0.0005),
    nrow = 1, dimnames = list(NULL, c("Est.", "SE"))
  )
  fit
}

test_that("qqplot.rga runs silently on valid input (numeric params)", {
  fit <- make_rga_numeric()
  expect_silent(qqplot.rga(fit))
  expect_silent(qqplot.rga(fit, main = "Custom QQ", pch = 19, col = "blue"))
})

test_that("qqplot.rga runs silently on valid input (matrix/list params)", {
  fit <- make_rga_matrix()
  expect_silent(qqplot.rga(fit))
})

test_that("ppplot.rga runs silently on valid input (numeric params)", {
  fit <- make_rga_numeric()
  expect_silent(ppplot.rga(fit))
  expect_silent(ppplot.rga(fit, main = "Custom PP", pch = 17, col = "red"))
})

test_that("ppplot.rga runs silently on valid input (matrix/list params)", {
  fit <- make_rga_matrix()
  expect_silent(ppplot.rga(fit))
})

# ── gof() tests ────────────────────────────────────────────────────────────────

test_that("gof() is an S3 generic that dispatches gof.rga", {
  fit <- make_rga_numeric()
  g <- gof(fit)
  expect_s3_class(g, "gof")
})

test_that("gof.rga returns correct structure", {
  fit <- make_rga_numeric()
  g <- gof(fit)
  expect_named(g, c("cvm", "ks", "n", "model_type"))
  expect_equal(g$model_type, "Crow-AMSAA")
  expect_equal(g$n, 5L)
  expect_true(is.numeric(g$cvm) && g$cvm > 0)
  expect_true(is.numeric(g$ks) && g$ks > 0 && g$ks <= 1)
})

test_that("gof.rga CvM is within expected range for small well-fitting dataset", {
  # For data generated from a known NHPP Power Law, statistics should be small
  set.seed(42)
  times <- cumsum(rexp(20, rate = 0.01))
  failures <- rep(1, 20)
  fit <- rga(times, failures, times_type = "cumulative_failure_times")
  g <- gof(fit)
  expect_true(g$cvm < 1.0)
  expect_true(g$ks < 1.0)
})

test_that("gof.rga errors on piecewise NHPP", {
  times <- c(5, 10, 15, 20, 25, 50, 60, 80, 90, 100)
  failures <- c(1, 2, 1, 3, 2, 1, 2, 1, 3, 2)
  fit <- rga(times, failures, model_type = "Piecewise NHPP")
  expect_error(gof(fit), "currently supports only the Crow-AMSAA model")
})

test_that("gof.rga errors on non-rga input", {
  expect_error(gof(list()), "'x' must be an object of class 'rga'.")
  expect_error(gof("text"), "'x' must be an object of class 'rga'.")
})

test_that("print.gof produces expected output", {
  fit <- make_rga_numeric()
  g <- gof(fit)
  out <- capture.output(print(g))
  expect_true(any(grepl("Cramer-von Mises", out)))
  expect_true(any(grepl("Kolmogorov-Smirnov", out)))
  expect_true(any(grepl("Crow-AMSAA", out)))
})

test_that("print.gof errors on non-gof input", {
  expect_error(print.gof(list()), "'x' must be an object of class 'gof'.")
})
