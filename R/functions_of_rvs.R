#' Generic Method to Map a Random Variable
#'
#' This function finds the distribution of a function of a random variable.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
mapRV = function(X, ...) {
  UseMethod("mapRV", X)
}

#' Find the Distribution of a Function of a Random Variable
#'
#' This function uses the distribution function technique to find the distribution
#'   of a differentiable function of a random variable and returns the result
#'   as an RV object.
#'
#' @param X An object of class "RV".
#' @param g A differentiable function.
#'
#' @return An object of class "RV", whose distribution is that of g(X).
#'
#' @export
mapRV.RV = function(X, g) {
  f = getCDF(X)
  inverse = function(y) {
    optim(y, function(x) {(g(x) - y)^2}, method = "BFGS")$par
  }
  h = function(y) {
    f(inverse(y))
  }
  Y = createRV(h,
               type = "CDF",
               lower = g(X[["lower"]]),
               upper = g(X[["upper"]]))
  Y
}

# TODO: infix operators (sum, product, difference, quotient), min, and max
# Sum and product have basic formulas
# Difference is mapRV with g(x) = -x and then a sum
# Quotient is mapRV with g(x) = 1/x and then a sum
# Min and max have basic formulas
# Order statistics have basic formulas

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
`%convolution%` = function(X, Y) {
  f = getPDF(X)
  g = getCDF(Y)
  h = function(z) {
    return(integrate(function(t) return(g(t) * f(z - t)), lower = -Inf, upper = Inf)[["value"]])
  }
  Z = structure(list(f = h,
                     type = "CDF",
                     lower = X[["lower"]] + Y[["lower"]],
                     upper = X[["upper"]] + Y[["upper"]]),
                class = "RV")
  Z
}

#'
#'
#'
`%product%` = function(X, Y) {
  stop("Not implemented yet.")
}

#'
#'
#'
`%difference%` = function(X, Y) {
  stop("Not implemented yet.")
}

#'
#'
#'
`%quotient%` = function(X, Y) {
  stop("Not implemented yet.")
}
