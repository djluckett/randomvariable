% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_variates.R
\name{generateVariates.RV}
\alias{generateVariates.RV}
\title{Generate Random Variates from A Random Variable Object}
\usage{
\method{generateVariates}{RV}(X, n)
}
\arguments{
\item{X}{An object of class "RV".}

\item{n}{A length one numeric. The number of random variates to generate.}
}
\value{
A numeric vector of length n, representing random variates following
  the distribution of X.
}
\description{
This function generates random variates from a distribution defined by
  a random variable object.
}
