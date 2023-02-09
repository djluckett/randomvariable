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
    PMF = function(x, ...) {
      if (!is.null(X[["support"]])) {
        if (x %in% X[["support"]]) {
          sorted_support = sort(X[["support"]])
          return(X[["f"]](x) - X[["f"]](sorted_support[which(sorted_support == x) - 1]))
        } else {
          return(0)
        }
      } else {
        if (x >= X[["lower"]] & x <= X[["upper"]] & x == floor(x)) {
          return(X[["f"]](x) - X[["f"]](x - 1))
        } else {
          return(0)
        }
      }
    }
  }
  function(x) sapply(x, PMF)
}

#' Get the CDF from a Discrete Random Variable Object
#'
#' This function extracts the CDF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the CDF of X.
#'
#' @export
getCDF.discreteRV = function(X) {
  if (X[["type"]] == "CDF") {
    CDF = function(x, ...) {
      if (!is.null(X[["support"]])) {
        if (x >= min(X[["support"]]) & x <= max(X[["support"]])) {
          return(X[["f"]](x))
        } else if (x < min(X[["support"]])) {
          return(0)
        } else {
          return(1)
        }
      } else {
        if (x >= X[["lower"]] & x <= X[["upper"]]) {
          return(X[["f"]](x))
        } else if (x < X[["lower"]]) {
          return(0)
        } else {
          return(1)
        }
      }
    }
  } else if (X[["type"]] == "PMF") {
    CDF = function(x, ...) {
      PMF = getPMF(X)
      if (!is.null(X[["support"]])) {
        return(sum(PMF(X[["support"]][X[["support"]] <= x])))
      } else {
        return(sum(PMF(seq(X[["lower"]], x, 1))))
      }
    }
  }
  function(x) sapply(x, CDF)
}
