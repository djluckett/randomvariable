test_that("Mapping a discrete random variable works", {
  # Create a custom discrete random variable
  pmf = function(x) {
    if (x == 1) {
      return(0.2)
    } else if (x == 2) {
      return(0.1)
    } else if (x == 3) {
      return(0.5)
    } else if (x == 4) {
      return(0.05)
    } else if (x == 5) {
      return(0.15)
    } else {
      return(0)
    }
  }
  X = discreteRV(f = pmf, type = "PMF", support = seq(1, 5))

  # Find the distribution of log(X)
  Y = mapRV(X, function(x) log(x))

  # Test that the support is correct
  expect_identical(Y[["support"]], log(X[["support"]]))

  # Test that the PMF sums to 1
  PMF = getPMF(Y)
  expect_equal(sum(PMF(Y[["support"]])), 1)
})

test_that("Mapping a discrete random variable defined on an interval works", {
  # Create a discrete random variable defined on an interval
  pmf = function(x) {
    if (x == 1) {
      return(0.2)
    } else if (x == 2) {
      return(0.1)
    } else if (x == 3) {
      return(0.5)
    } else if (x == 4) {
      return(0.05)
    } else if (x == 5) {
      return(0.15)
    } else {
      return(0)
    }
  }
  X = discreteRV(f = pmf, type = "PMF", lower = 1, upper = 5)

  # Find the distribution of log(X)
  Y = mapRV(X, function(x) log(x))

  # Test that the support is correct
  expect_identical(Y[["support"]], log(seq(X[["lower"]], X[["upper"]])))

  # Test that the PMF sums to 1
  PMF = getPMF(Y)
  expect_equal(sum(PMF(Y[["support"]])), 1)
})

test_that("Convolutions of discrete random variables work", {
  # Create a custom discrete random variable
  first_pmf = function(x) {
    if (x == 1) {
      return(0.2)
    } else if (x == 2) {
      return(0.1)
    } else if (x == 3) {
      return(0.5)
    } else if (x == 4) {
      return(0.05)
    } else if (x == 5) {
      return(0.15)
    } else {
      return(0)
    }
  }
  X = discreteRV(f = first_pmf, type = "PMF", support = seq(1, 5))

  # Create a second custom discrete random variable
  second_pmf = function(y) {
    if (y == 0) {
      return(0.5)
    } else if (y == 2) {
      return(0.1)
    }else if (y == 4) {
      return(0.4)
    } else {
      return(0)
    }
  }
  Y = discreteRV(f = second_pmf, type = "PMF", support = c(0, 2, 4))

  # Find the distribution of X + Y
  Z = X %convolution% Y

  # Test that the PMF sums to 1
  PMF = getPMF(Z)
  expect_equal(sum(PMF(Z[["support"]])), 1)
})
