#' Apply a Function to a Random Variable
#'
#' This is a generic function to find the distribution of a function
#'   of a random variable.
#'
#' @param X A random variable object.
#'
#' @return Another random variable object.
#'
#' @export
mapRV = function(X, ...) {
  UseMethod("mapRV", X)
}

#' Find the Distribution of a Function of a Random Variable
#'
#' This function uses the distribution function technique to find the distribution
#'   of a differentiable function of a random variable and returns the result
#'   as an RV object. This function is still experimental.
#'
#' @param X An object of class "RV".
#' @param g A differentiable function.
#'
#' @return An object of class "RV", whose distribution is that of g(X).
#'
#' @export
mapRV.RV = function(X, g) {
  CDF = getCDF(X)
  inverse = function(y) {
    # optim(y, function(x) {(g(x) - y)^2}, method = "BFGS")$par
    uniroot(function(x) return(g(x) - y), interval = c(max(-5000, X[["lower"]]), min(5000, X[["upper"]])))$root
  }
  mapped_CDF = function(y) {
    CDF(inverse(y))
  }
  new_RV(list(f = mapped_CDF,
              type = "CDF",
              lower = min(g(X[["lower"]]), g(X[["upper"]])),
              upper = max(g(X[["lower"]]), g(X[["upper"]]))))
}

#' Find the Distribution of a Convolution
#'
#' This is a generic function to find the distribution of the convolution of
#'   two objects.
#'
#' @param X An object with a distribution.
#' @param Y An object with a distribution.
#'
#' @return An object of the same class as X, whose distribution is
#'   that of X + Y.
#'
#' @export
`%convolution%` = function(X, Y) {
  UseMethod("convolution")
}

#' Find the Distribution of a Convolution of Random Variables
#'
#' This function finds the distribution of a convolution of two random variables.
#'   The two random variables are assumed to be independent. The result is
#'   returned as an object of class "RV".
#'
#' @param X An object of class "RV".
#' @param Y An object of class "RV".
#'
#' @return An object of class "RV", whose distribution is that of X + Y.
#'
#' @export
`%convolution%.RV` = function(X, Y) {
  PDF = getPDF(X)
  CDF = getCDF(Y)
  convolution_CDF = function(z) {
    return(integrate(function(t) return(CDF(t) * PDF(z - t)), lower = -Inf, upper = Inf)[["value"]])
  }
  new_RV(list(f = convolution_CDF,
              type = "CDF",
              lower = X[["lower"]] + Y[["lower"]],
              upper = X[["upper"]] + Y[["upper"]]))
}

#' Find the Distribution of a Product of Random Variables
#'
#' This function finds the distribution of a product of two random variables.
#'   The two random variables are assumed to be independent. The result is
#'   returned as an object of class "RV". This function is still experimental.
#'
#' @param X An object of class "RV".
#' @param Y An object of class "RV".
#'
#' @return An object of class "RV", whose distribution is that of X * Y.
#'
#' @export
`%product%` = function(X, Y) {
  first_PDF = getPDF(X)
  second_PDF = getPDF(Y)
  product_PDF = function(z) {
    integrate(function(x) {first_PDF(x) * second_PDF(z / x) / abs(x)},
              lower = max(z / Y[["upper"]], X[["lower"]]),
              upper = min(z / Y[["lower"]], X[["upper"]]))
  }
  new_RV(list(f = product_PDF,
              type = "PDF",
              lower = min(c(X[["lower"]] * Y[["lower"]],
                            X[["upper"]] * Y[["upper"]],
                            X[["lower"]] * Y[["upper"]],
                            X[["upper"]] * Y[["lower"]])),
              upper = max(c(X[["lower"]] * Y[["lower"]],
                            X[["upper"]] * Y[["upper"]],
                            X[["lower"]] * Y[["upper"]],
                            X[["upper"]] * Y[["lower"]]))))
}

#' Find the Distribution of a Difference of Random Variables
#'
#' This function finds the distribution of a difference of two random variables.
#'   The two random variables are assumed to be independent. The result is
#'   returned as an object of class "RV".
#'
#' @param X An object of class "RV".
#' @param Y An object of class "RV".
#'
#' @return An object of class "RV", whose distribution is that of X - Y.
#'
#' @export
`%difference%` = function(X, Y) {
  negative_Y = mapRV(Y, function(y) return(-y))
  X %convolution% negative_Y
}

#' Find the Distribution of a Quotient of Random Variables
#'
#' This function finds the distribution of a quotient of two random variables.
#'   The two random variables are assumed to be independent. The result is
#'   returned as an object of class "RV". This function is still experimental.
#'
#' @param X An object of class "RV".
#' @param Y An object of class "RV".
#'
#' @return An object of class "RV", whose distribution is that of X / Y.
#'
#' @export
`%quotient%` = function(X, Y) {
  reciprocal_Y = mapRV(Y, function(y) return(1/y))
  X %product% reciprocal_Y
}
