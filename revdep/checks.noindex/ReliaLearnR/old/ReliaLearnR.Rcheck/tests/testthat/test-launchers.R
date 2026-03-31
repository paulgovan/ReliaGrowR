test_that("ram runs without error in interactive mode", {
  skip_if_not(interactive(), "Only runs interactively")
  expect_error(ram(), NA)
})

test_that("lda runs without error in interactive mode", {
  skip_if_not(interactive(), "Only runs interactively")
  expect_error(lda(), NA)
})

test_that("rt runs without error in interactive mode", {
  skip_if_not(interactive(), "Only runs interactively")
  expect_error(rt(), NA)
})
