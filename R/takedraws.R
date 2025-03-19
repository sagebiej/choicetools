#' Draw Samples from a Multivariate Normal Distribution
#'
#' This function generates random draws from a multivariate normal distribution
#' given a point estimate (`beta`) and a variance-covariance matrix (`vc`).
#' It uses the Cholesky decomposition for efficient sampling.
#'
#' @param n Integer. The number of samples to draw (default: `10000`).
#' @param beta Numeric vector. The mean (point estimate) of the distribution.
#' @param vc Numeric matrix. The variance-covariance matrix of the distribution.
#'
#' @return A numeric matrix of dimension `n x length(beta)`, where each row is a draw
#'         from the multivariate normal distribution.
#'
#' @examples
#' beta <- c(1.5, 2.0)
#' vc <- matrix(c(0.1, 0.02, 0.02, 0.2), nrow = 2)
#' draws <- takedraws(n = 1000, beta, vc)
#' head(draws)
#'
#' @export
takedraws <-function(n=10000, beta,vc) {
  
  
  # Ensure beta is a column vector
  k <- length(beta)
  
  # Cholesky decomposition
  cholesky <- chol(vc)  # Decomposes vc into L such that L %*% t(L) = vc
  
  # Generate standard normal random variables in one step
  z <- matrix(rnorm(n * k), nrow = k, ncol = n)
  
  # Transform using Cholesky decomposition (vectorized)
  draws <- t(beta + t(cholesky) %*% z)
  
  # Set column names
  colnames(draws) <- names(beta)
  
  return(draws)
}