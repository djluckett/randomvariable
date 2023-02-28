#' Find the Distribution of a Function of a Discrete Random Variable
#'
#' This function finds the distribution of a differentiable function of a
#'   discrete random variable and returns the result as a discreteRV object.
#'   This function is still experimental.
#'
#' @param X An object of class "discreteRV".
#' @param g A differentiable function.
#'
#' @return An object of class "discreteRV", whose distribution is that of g(X).
#'
#' @export
mapRV.discreteRV = function(X, g) {
  if (!is.null(X[["support"]])) {
    PMF = getPMF(X)
    inverse = function(y) {
      # optim(y, function(x) {(g(x) - y)^2}, method = "BFGS")$par
      uniroot(function(x) return(g(x) - y),
              interval = c(max(-5000, min(X[["support"]])),
                           min(5000, max(X[["support"]]))))$root
    }
    mapped_PMF = function(y) {
      x = inverse(y)
      if (x %in% X[["support"]]) {
        return(PMF(x))
      } else {
        return(0)
      }
    }
    support = sapply(X[["support"]], g)
    new_RV = new_discreteRV(list(f = mapped_PMF,
                                 type = "PMF",
                                 support = support))
  } else {
    PMF = getPMF(X)
    inverse = function(y) {
      # optim(y, function(x) {(g(x) - y)^2}, method = "BFGS")$par
      uniroot(function(x) return(g(x) - y),
              interval = c(max(-5000, min(X[["support"]])),
                           min(5000, max(X[["support"]]))))$root
    }
    mapped_PMF = function(y) {
      x = inverse(y)
      if (x %in% X[["support"]]) {
        return(PMF(x))
      } else {
        return(0)
      }
    }
    lower = min(g(X[["lower"]]), g(X[["upper"]]))
    upper = max(g(X[["lower"]]), g(X[["upper"]]))
    new_RV = new_discreteRV(list(f = mapped_PMF,
                                 type = "PMF",
                                 lower = lower,
                                 upper = upper))
  }
  new_RV
}

#' Find the Distribution of a Convolution of Discrete Random Variables
#'
#' This function finds the distribution of a convolution of two discrete
#'   random variables. The two discrete random variables are assumed to be
#'   independent. The result is returned as an object of class "discreteRV".
#'
#' @param X An object of class "discreteRV".
#' @param Y An object of class "discreteRV".
#'
#' @return An object of class "discreteRV", whose distribution is that of X + Y.
#'
#' @export
`%convolution%.discreteRV` = function(X, Y) {
  # TODO: fill in discrete random variable convolutions
}

