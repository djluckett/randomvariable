#' Internal Constructor for Discrete Random Variables
#'
#' This is the internal construction function for objects of class "discreteRV".
#'
#' @param X A list, to be used to construct a discrete random variable.
#'
#' @return An object of class "discreteRV".
new_discreteRV = function(X = list()) {
  stopifnot(is.list(X))
  structure(X, class = "discreteRV")
}

#' Validate Discrete Random Variable Objects
#'
#' This function checks whether a discrete random variable object is valid.
#'
#' @param X An object of class "discreteRV".
#'
#' @return X. Primarily called for the side effect of producing an error message
#'   if the object is not a valid discrete random variable.
validate_discreteRV = function(X) {

  # Check class and structure of object
  if (!("discreteRV" %in% class(X))) {
    stop("Attempting to validate an object that is not a discrete random variable.")
  }
  if (!is.list(X)) {
    stop("A discrete random variable object must be a list.")
  }

  # Check existence of all necessary elements
  if (is.null(X[["f"]])) {
    stop("A discrete random variable must have a defining function.")
  }
  if (is.null(X[["type"]])) {
    stop("The type of the defining function must be specified.")
  }
  if (is.null(X[["support"]]) & (is.null(X[["lower"]]) | is.null(X[["upper"]]))) {
    stop("The support must be specified, either using support or both upper and lower.")
  }

  # Check type of all elements
  if (!is.function(X[["f"]])) {
    stop("The defining function must be a function.")
  }
  if (!is.character(X[["type"]])) {
    stop("Defining function type must be a string.")
  }
  if (!is.null(X[["support"]])) {
    if (!is.numeric(X[["support"]]) & !is.integer(X[["support"]])) {
      stop("Support must be a numeric or integer vector.")
    }
  } else {
    if (!(is.numeric(X[["lower"]]) | is.integer(X[["lower"]])) |
        !(is.numeric(X[["upper"]] | is.integer(X[["upper"]])))) {
      stop("Lower and upper limits must be numeric or integer.")
    }
  }

  # Check that lower limit is less than upper limit (if support is NULL)
  if (is.null(X[["support"]])) {
    if (X[["lower"]] >= X[["upper"]]) {
      stop("Lower limit must be less than upper limit.")
    }
  }

  # Check that defining function is valid and a supported type
  if (X[["type"]] == "PMF") {
    # TODO: check that PMF is valid
  } else if (X[["type"]] == "CDF") {
    # TODO: check that CDF is valid
  } else {
    stop("Unrecognized defining function type. Only 'PDF' and 'CDF' are currently supported.")
  }

  X
}

#' Discrete Random Variables
#'
#' This function initializes a discrete random variable object using a defining
#'   function, an indicator of what type the defining function is ("PMF", "CDF", etc.),
#'   and a description of the support. The support can be given either as a vector of
#'   values or as upper and lower limits. If upper and lower limits are supplied,
#'   the support is assumed to be all integers between the upper and lower limits,
#'   inclusive. Currently, this supports univariate, discrete random variables.
#'
#' @param f A function, where f(x) returns the value of the defining function at x.
#' @param type The type of function that the defining function is ("PMF", "CDF", etc.).
#'   Defaults to "PMF".
#' @param lower The lower limit of the support of the distribution. Defaults to 0.
#' @param upper The upper limit of the support of the distribution. Defaults to Inf.
#' @param support A vector of numeric or integer values where the PMF is positive.
#'   If support is not NULL, upper and lower will be ignored. If support is NULL,
#'   upper and lower will be used to define the support. Defaults to NULL.
#'
#' @return An object of class "discreteRV".
#'
#' @export
discreteRV = function(f, type = "PMF", lower = 0, upper = Inf, support = NULL) {
  X = new_discreteRV(list(f = f, type = type, lower = lower, upper = upper, support = support))
  validate_discreteRV(X)
}

