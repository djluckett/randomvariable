#' Get a probability mass function
#'
#' This is a generic function to get the probability mass function from
#'   an object.
#'
#' @param X An object for which a probability mass function can be defined.
#'
#' @return A function.
#'
#' @export
getPMF = function(X, ...) {
  UseMethod("getPMF", X)
}

#' Get the PMF from a Discrete Random Variable Object
#'
#' This function extracts the PMF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the PMF of X.
#'
#' @export
getPMF.discreteRV = function(X) {
  if (X[["type"]] == "PMF") {
    PMF = function(x, ...) {
      if (!is.null(X[["support"]])) {
        if (x %in% X[["support"]]) {
          return(X[["f"]](x))
        } else {
          return(0)
        }
      } else {
        if (x >= X[["lower"]] & x <= X[["upper"]] & x == floor(x)) {
          return(X[["f"]](x))
        } else {
          return(0)
        }
      }
    }
  } else if (X[["type"]] == "CDF") {
    # TODO: derive PMF from CDF for discrete distribution
  }
  function(x) sapply(x, PMF)
}
