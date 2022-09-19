#' Generic Method to Get a PDF
#'
#' This function extracts the PDF from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getPDF = function(X, ...) {
  UseMethod("getPDF", X)
}

#' Generic Method to Get a CDF
#'
#' This function extracts the CDF from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getCDF = function(X, ...) {
  UseMethod("getCDF", X)
}

#' Generic Method to Get an Inverse CDF
#'
#' This function extracts the inverse CDF from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getInverseCDF = function(X, ...) {
  UseMethod("getInverseCDF", X)
}

#' Generic Method to Get a Survival Function
#'
#' This function extracts the survival function from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getSF = function(X, ...) {
  UseMethod("getSF", X)
}

#' Generic Method to Get a Hazard Function
#'
#' This function extracts the hazard function from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getHF = function(X, ...) {
  UseMethod("getHF", X)
}

#' Generic Method to Get a Cumulative Hazard Function
#'
#' This function extracts the cumulative hazard function from an object.
#'
#' @param X An object.
#'
#' @return An object.
#'
#' @export
getCHF = function(X, ...) {
  UseMethod("getCHF", X)
}

#' Generic Method to Get a Moment Generating Function
#'
#' This function extracts the moment generating function from an object.
#'
#' @param X An object.
#'
#' @return An object.
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
      integrate(X[["f"]], lower = X[["lower"]], upper = x, ...)[["value"]]
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
