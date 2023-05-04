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
      if (any(x - X[["support"]] < 0.0001)) {
        return(PMF(X[["support"]][abs(x - X[["support"]]) < 0.0001]))
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
              interval = c(max(-5000, X[["lower"]]),
                           min(5000, X[["upper"]])))$root
    }
    X_support = seq(max(-5000, X[["lower"]]), min(5000, X[["upper"]]))
    mapped_PMF = function(y) {
      x = inverse(y)
      if (any(x - X_support < 0.0001)) {
        return(PMF(X_support[abs(x - X_support) < 0.0001]))
      } else {
        return(0)
      }
    }
    support = sapply(X_support, g)
    new_RV = new_discreteRV(list(f = mapped_PMF,
                                 type = "PMF",
                                 support = support))
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
  if (!is.null(X[["support"]]) & !is.null(Y[["support"]])) {
    first_PMF = getPMF(X)
    second_PMF = getPMF(Y)
    support_df = data.frame(x = rep(X[["support"]], each = length(Y[["support"]])),
                            y = rep(Y[["support"]], times = length(X[["support"]])))
    support_df[["z"]] = support_df[["x"]] + support_df[["y"]]
    convolution_PMF = function(z) {
      if (!(z %in% support_df[["z"]])) {
        return(0)
      } else {
        z_support = support_df[support_df[["z"]] == z, ]
        z_support[["prob"]] = first_PMF(z_support[["x"]]) * second_PMF(z_support[["y"]])
        return(sum(z_support[["prob"]]))
      }
    }
    new_RV = new_discreteRV(list(f = convolution_PMF,
                                 type = "PMF",
                                 support = sort(unique(support_df[["z"]]))))
  } else {
    stop("Convolutions of discrete random variables are currently only supported for two random variables with defined support.")
  }
  new_RV
}

