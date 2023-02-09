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

#' Get the Inverse CDF from a Discrete Random Variable Object
#'
#' This function extracts the Inverse CDF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the Inverse CDF of X.
#'
#' @export
getInverseCDF.discreteRV = function(X) {
  inverse_CDF = function(x, ...) {
    CDF = getCDF(X)
    if (!is.null(X[["support"]])) {
      sorted_support = sort(support)
      prob = 0
      i = 1
      while (prob <= y) {
        x = sorted_support[i]
        prob = CDF(x)
        i = i + 1
      }
      return(x)
    } else {
      prob = 0
      x = X[["lower"]]
      while(prob <= y) {
        prob = CDF(x)
        x = x + 1
      }
      return(x)
    }
  }
  function(x) sapply(x, inverse_CDF)
}

#' Get the Survival Function from a Discrete Random Variable Object
#'
#' This function extracts the SF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the SF of X.
#'
#' @export
getSF.discreteRV = function(X) {
  CDF = getCDF(X)
  SF = function(x) {
    return(1 - CDF(x))
  }
  function(x) sapply(x, SF)
}

#' Get the Hazard Function from a Discrete Random Variable Object
#'
#' This function extracts the HF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the HF of X.
#'
#' @export
getHF.discreteRV = function(X) {
  PMF = getPMF(X)
  SF = getSF(X)
  HF = function(x) {
    return(PDF(x) / SF(x))
  }
  function(x) sapply(x, HF)
}

#' Get the Cumulative Hazard Function from a Discrete Random Variable Object
#'
#' This function extracts the CHF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the CHF of X.
#'
#' @export
getCHF.discreteRV = function(X) {
  HF = getHF(X)
  CHF = function(x, ...) {
    if (!is.null(X[["support"]])) {
      return(sum(HF(X[["support"]][X[["support"]] <= x])))
    } else {
      return(sum(HF(seq(X[["lower"]], x, 1))))
    }
  }
  function(x) sapply(x, CHF)
}

#' Get the Moment Generating Function from a Discrete Random Variable Object
#'
#' This function extracts the MGF from a discrete random variable object.
#'
#' @param X An object of class "discreteRV".
#'
#' @return A function that evaluates the MGF of X.
#'
#' @export
getMGF.discreteRV = function(X) {
  PMF = getPMF(X)
  MGF = function(t) {
    if (!is.null(X[["support"]])) {
      return(sum(sapply(X[["support"]], function(x) return(exp(t * x) * PMF(x)))))
    } else {
      return(sum(sapply(seq(max(X[["lower"]], -5000), min(X[["upper"]], 5000), 1),
                        function(x) return(exp(t * x) * PMF(x)))))
    }
  }
  function(x) sapply(x, MGF)
}
