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
