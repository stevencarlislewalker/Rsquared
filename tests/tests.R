library(testthat)

test_that("R2 consistent across object classes",{
  library(Rsquared)
  library(popmoments)
  set.seed(1)
  x <- rnorm(10)
  y <- rnorm(10)
  m <- lm(y~x)
  expect_that(R2(y,x), equals(R2(m), tolerance = 10e-3))
  expect_that(R2adj(y,x), equals(R2adj(m), tolerance = 10e-3))
  expect_that(R2press(y,x), equals(R2press(m), tolerance = 10e-3))
  expect_that(R2gcv(y,x), equals(R2gcv(m), tolerance = 10e-3))
})
