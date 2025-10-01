
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
