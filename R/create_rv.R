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
  X = list(f = f, type = type, lower = lower, upper = upper)
  class(X) = "RV"
  X
}
