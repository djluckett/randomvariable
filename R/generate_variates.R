#' Generic Method to Generate Random Variates
#'
#' This function generates random variates from an object.
#'
#' @param X An object.
#'
#' @return An object.
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
#' @return A numeric vector of length n.
#'
#' @export
generateVariates.RV = function(X, n) {
  inverseCDF = getInverseCDF(X)
  unifs = runif(n)
  sapply(unifs, inverseCDF)
}
