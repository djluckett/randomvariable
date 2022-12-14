% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_of_rvs.R
\name{\%quotient\%}
\alias{\%quotient\%}
\title{Find the Distribution of a Quotient of Random Variables}
\usage{
X \%quotient\% Y
}
\arguments{
\item{X}{An object of class "RV".}

\item{Y}{An object of class "RV".}
}
\value{
An object of class "RV", whose distribution is that of X / Y.
}
\description{
This function finds the distribution of a quotient of two random variables.
  The two random variables are assumed to be independent. The result is
  returned as an object of class "RV". This function is still experimental.
}
