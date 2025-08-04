library(testthat)

# Load the function


test_that("takedraws generates the correct output dimensions", {
  beta <- c(1.5, 2.0)
  vc <- matrix(c(0.1, 0.02, 0.02, 0.2), nrow = 2)

  draws <- takedraws(n = 1000, beta, vc)

  expect_true(is.matrix(draws))
  expect_equal(dim(draws), c(1000, length(beta)))
})

test_that("takedraws retains correct column names", {
  beta <- c(a = 1.5, b = 2.0)
  vc <- matrix(c(0.1, 0.02, 0.02, 0.2), nrow = 2)

  draws <- takedraws(n = 500, beta, vc)

  expect_true(all(colnames(draws) == names(beta)))
})

test_that("takedraws produces approximately normal output", {
  beta <- c(1.5, 2.0)
  vc <- matrix(c(0.1, 0.02, 0.02, 0.2), nrow = 2)

  draws <- takedraws(n = 10000, beta, vc)

  # Check that sample mean is close to beta
  expect_true(all(abs(colMeans(draws) - beta) < 0.1))

  # Check that sample covariance is close to vc
  sample_cov <- cov(draws)
  expect_true(all(abs(sample_cov - vc) < 0.05))
})

test_that("takedraws handles edge cases gracefully", {
  beta <- c(1.5)
  vc <- matrix(0.1, nrow = 1, ncol = 1)

  draws <- takedraws(n = 1000, beta, vc)

  expect_equal(dim(draws), c(1000, 1))
  expect_true(all(colnames(draws) == names(beta)))
})

test_that("takedraws errors for invalid inputs", {
  beta <- c(1.5, 2.0)
  vc_wrong <- matrix(c(0.1, 0.02), nrow = 1) # Not square

  expect_error(takedraws(n = 1000, beta, vc_wrong), "must be a square matrix")

  vc_negative_var <- matrix(c(-0.1, 0.02, 0.02, 0.2), nrow = 2)
  expect_error(takedraws(n = 1000, beta, vc_negative_var), "the leading minor of order 1 is not positive")
})
