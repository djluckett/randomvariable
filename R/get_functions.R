#' Get a probability density function
#'
#' This is a generic function to get the probability density function from
#'   an object.
#'
#' @param X An object for which a probability density function can be defined.
#'
#' @return A function.
#'
#' @export
getPDF = function(X, ...) {
  UseMethod("getPDF", X)
}

#' Get a cumulative distribution function
#'
#' This is a generic function to get the cumulative distribution function from
#'   an object.
#'
#' @param X An object for which a cumulative distribution function can be defined.
#'
#' @return A function.
#'
#' @export
getCDF = function(X, ...) {
  UseMethod("getCDF", X)
}

#' Get an inverse cumulative distribution function
#'
#' This is a generic function to get the inverse cumulative distribution
#'   function from an object.
#'
#' @param X An object for which an inverse cumulative distribution function can be defined.
#'
#' @return A function.
#'
#' @export
getInverseCDF = function(X, ...) {
  UseMethod("getInverseCDF", X)
}

#' Get a survival function
#'
#' This is a generic function to get the survival function from an object.
#'
#' @param X An object for which a survival function can be defined.
#'
#' @return A function.
#'
#' @export
getSF = function(X, ...) {
  UseMethod("getSF", X)
}

#' Get a hazard function
#'
#' This is a generic function to get the hazard function from an object.
#'
#' @param X An object for which a hazard function can be defined.
#'
#' @return A function.
#'
#' @export
getHF = function(X, ...) {
  UseMethod("getHF", X)
}

#' Get a cumulative hazard function
#'
#' This is a generic function to get the cumulative hazard function from
#'   an object.
#'
#' @param X An object for which a cumulative hazard function can be defined.
#'
#' @return A function.
#'
#' @export
getCHF = function(X, ...) {
  UseMethod("getCHF", X)
}

#' Get a moment generating function
#'
#' This is a generic function to get the moment generating function from
#'   an object.
#'
#' @param X An object for which a moment generating function can be defined.
#'
#' @return A function.
#'
#' @export
getMGF = function(X, ...) {
  UseMethod("getMGF", X)
}

#' Get the PDF from a Random Variable Object
#'
#' This function extracts the PDF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the PDF of X.
#'
#' @export
getPDF.RV = function(X) {
  if (X[["type"]] == "PDF") {
    PDF = function(x, ...) {
      if (x >= X[["lower"]] & x <= X[["upper"]]) {
        X[["f"]](x)
      } else {
        0
      }
    }
  } else if (X[["type"]] == "CDF") {
    PDF = function(x, ...) {
      if (x >= X[["lower"]] & x <= X[["upper"]]) {
        numDeriv::grad(X[["f"]], x)
      } else {
        0
      }
    }
  }
  function(x) sapply(x, PDF)
}

#' Get the CDF from a Random Variable Object
#'
#' This function extracts the CDF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the CDF of X.
#'
#' @export
getCDF.RV = function(X) {
  if (X[["type"]] == "CDF") {
    CDF = function(x) {
      if (x < X[["lower"]]) {
        return(0)
      } else if (x > X[["upper"]]) {
        return(1)
      } else {
        return(X[["f"]](x))
      }
    }
  } else if (X[["type"]] == "PDF") {
    CDF = function(x, ...) {
      if (x < X[["lower"]]) {
        return(0)
      } else if (x > X[["upper"]]) {
        return(1)
      } else {
        integrate(X[["f"]], lower = X[["lower"]], upper = x, ...)[["value"]]
      }
    }
  }
  function(x) sapply(x, CDF)
}

#' Get the Inverse CDF from a Random Variable Object
#'
#' This function extracts the Inverse CDF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the Inverse CDF of X.
#'
#' @export
getInverseCDF.RV = function(X) {
  if (X[["type"]] == "CDF") {
    inverse_CDF = function(x, ...) {
      GoFKernel::inverse(X[["f"]])(x)
    }
  } else if (X[["type"]] == "PDF") {
    inverse_CDF = function(x, ...) {
      GoFKernel::inverse(getCDF(X))(x)
    }
  }
  function(x) sapply(x, inverse_CDF)
}

#' Get the Survival Function from a Random Variable Object
#'
#' This function extracts the SF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the SF of X.
#'
#' @export
getSF.RV = function(X) {
  CDF = getCDF(X)
  SF = function(x) {
    return(1 - CDF(x))
  }
  function(x) sapply(x, SF)
}

#' Get the Hazard Function from a Random Variable Object
#'
#' This function extracts the HF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the HF of X.
#'
#' @export
getHF.RV = function(X) {
  PDF = getPDF(X)
  SF = getSF(X)
  HF = function(x) {
    return(PDF(x) / SF(x))
  }
  function(x) sapply(x, HF)
}

#' Get the Cumulative Hazard Function from a Random Variable Object
#'
#' This function extracts the CHF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the CHF of X.
#'
#' @export
getCHF.RV = function(X) {
  HF = getHF(X)
  CHF = function(x, ...) {
    integrate(HF, lower = X[["lower"]], upper = x, ...)[["value"]]
  }
  function(x) sapply(x, CHF)
}

#' Get the Moment Generating Function from a Random Variable Object
#'
#' This function extracts the MGF from a random variable object.
#'
#' @param X An object of class "RV".
#'
#' @return A function that evaluates the MGF of X.
#'
#' @export
getMGF.RV = function(X) {
  PDF = getPDF(X)
  MGF = function(t) {
    integrate(function(x) return(exp(t * x) * PDF(x)), lower = X[["lower"]], upper = X[["upper"]])[["value"]]
  }
  function(x) sapply(x, MGF)
}
