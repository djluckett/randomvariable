#' Calculate Expected Value
#'
#' This function calculates the expected value of an object.
#'
#' @param X A random variable object.
#'
#' @return The expected value.
#'
#' @export
expectedValue = function(X, ...) {
  UseMethod("expectedValue", X)
}

#' Calculate Variance
#'
#' This function calculates the variance of an object.
#'
#' @param X A random variable object.
#'
#' @return The variance
#'
#' @export
variance = function(X, ...) {
  UseMethod("variance", X)
}

#' Calculate Expected Value of A Random Variable
#'
#' This function calculates the expected value of a random variable.
#'
#' @param X An object of class "RV".
#'
#' @return A length one numeric, the expected value of the random variable X.
#'
#' @export
expectedValue.RV = function(X) {
  PDF = getPDF(X)
  integrate(function(x) return(x * PDF(x)), lower = X[["lower"]], upper = X[["upper"]])[["value"]]
}

#' Calculate the Variance of A Random Variable
#'
#' This function calculates the variance of a random variable.
#'
#' @param X An object of class "RV".
#'
#' @return A length one numeric, the variance of the random variable X.
#'
#' @export
variance.RV = function(X) {
  PDF = getPDF(X)
  mu = expectedValue(X)
  integrate(function(x) return((x - mu)^2 * PDF(x)), lower = X[["lower"]], upper = X[["upper"]])[["value"]]
}
