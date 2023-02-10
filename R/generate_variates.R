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

#' Generate Random Variates from A Discrete Random Variable Object
#'
#' This function generates random variates from a distribution defined by
#'   a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#' @param n A length one numeric. The number of random variates to generate.
#'
#' @return A numeric vector of length n, representing random variates following
#'   the distribution of X.
#'
#' @export
generateVariates.discreteRV = function(X, n) {
  if (!is.null(X[["support"]])) {
    support = X[["support"]]
  } else {
    support = seq(max(X[["lower"]], -5000), min(X[["upper"]], 5000), 1)
  }
  PMF = getPMF(X)
  prob = PMF(support)
  sample(support, n, replace = TRUE, prob = prob)
}
