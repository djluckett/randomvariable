#' Generate Random Variates
#'
#' This is a generic function to generate random variates from a distribution.
#'
#' @param X A random variable object.
#'
#' @return A numeric vector of random variates.
#'
#' @export
generateVariates = function(X, ...) {
  UseMethod("generateVariates", X)
}

#' Generate Random Variates from A Random Variable Object
#'
#' This function generates random variates from a distribution defined by
#'   a random variable object.
#'
#' @param X An object of class "RV".
#' @param n A length one numeric. The number of random variates to generate.
#'
#' @return A numeric vector of length n, representing random variates following
#'   the distribution of X.
#'
#' @export
generateVariates.RV = function(X, n) {
  inverse_CDF = getInverseCDF(X)
  unifs = runif(n)
  sapply(unifs, inverse_CDF)
}
