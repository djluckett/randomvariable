#' Internal Constructor for Random Variables
#'
#' This is the internal construction function for objects of class "RV".
#'
#' @param X A list, to be used to construct a random variable.
#'
#' @return An object of class "RV".
new_RV = function(X = list()) {
  stopifnot(is.list(X))
  structure(X, class = "RV")
}

#' Validate Random Variable Objects
#'
#' This function checks whether a random variable object is valid.
#'
#' @param X An object of class "RV".
#'
#' @return X. Primarily called for the side effect of producing an error message
#'   if the object is not a valid random variable.
validate_RV = function(X) {

  # Check class and structure of object
  if (!("RV" %in% class(X))) {
    stop("Attempting to validate an object that is not a random variable.")
  }
  if (!is.list(X)) {
    stop("A random variable object must be a list.")
  }

  # Check existence of all necessary elements
  if (is.null(X[["f"]])) {
    stop("A random variable must have a defining function.")
  }
  if (is.null(X[["type"]])) {
    stop("The type of the defining function must be specified.")
  }
  if (is.null(X[["lower"]])) {
    stop("The lower limit of the support must be specified.")
  }
  if (is.null(X[["upper"]])) {
    stop("The upper limit of the support must be specified.")
  }

  # Check type of all elements
  if (!is.function(X[["f"]])) {
    stop("The defining function must be a function.")
  }
  if (!is.character(X[["type"]])) {
    stop("Defining function type must be a string.")
  }
  if (!is.numeric(X[["lower"]]) | !is.numeric(X[["upper"]])) {
    stop("Lower and upper limits must be numeric.")
  }

  # Check that lower limit is less than upper limit
  if (X[["lower"]] >= X[["upper"]]) {
    stop("Lower limit must be less than upper limit.")
  }

  # Check that defining function is valid and a supported type
  if (type == "PDF") {
    if (integrate(function(x) return(X[["f"]](x)), lower = X[["lower"]], upper = X[["upper"]])[["value"]] != 1) {
      stop("Invalid PDF supplied.")
    }
  } else if (type == "CDF") {
    if (X[["f"]](X[["lower"]]) != 0 | X[["f"]](X[["upper"]]) != 1) {
      stop("Invalid CDF supplied.")
    }
  } else {
    stop("Unrecognized defining function type. Only 'PDF' and 'CDF' are currently supported.")
  }

  X
}

#' Random Variables
#'
#' This function initializes a random variable object using a defining function,
#'   an indicator of what type the defining function is ("PDF", "CDF", etc.),
#'   and upper and lower limits of the support. Currently, this supports
#'   univariate, continuous random variables where the support is an interval.
#'
#' @param f A function, where f(x) returns the value of the defining function at x.
#' @param type The type of function that the defining function is ("PDF", "CDF", etc.).
#' @param lower The lower limit of the support of the distribution. Defaults to -Inf.
#' @param upper The upper limit of the support of the distribution. Defaults to Inf.
#'
#' @return An object of class "RV".
#'
#' @export
RV = function(f, type = "PDF", lower = -Inf, upper = Inf) {
  X = new_RV(list(f = f, type = type, lower = lower, upper = upper))
  validate_RV(X)
}

#' Create a Random Variable Object
#'
#' This function initializes a random variable object using a defining function
#'   and an indicator of what type the defining function is ("PDF", "CDF", etc.).
#'   Currently, this supports univariate, continuous random variables where the
#'   support is an interval.
#'
#' @param f A function, where f(x) returns the value of the defining function at x.
#' @param type The type of function that the defining function is ("PDF", "CDF", etc.).
#' @param lower The lower limit of the support of the distribution. Defaults to -Inf.
#' @param upper The upper limit of the support of the distribution. Defaults to Inf.
#'
#' @return An object of class "RV".
#'
#' @export
createRV = function(f, type = "PDF", lower = -Inf, upper = Inf) {
  .Deprecated("RV")
  X = list(f = f, type = type, lower = lower, upper = upper)
  class(X) = "RV"
  X
}
