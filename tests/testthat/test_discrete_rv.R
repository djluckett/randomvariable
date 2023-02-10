test_that("We can create a Bernoulli random variable", {
  # Test the discreteRV class by defining a Bernoulli random variable
  bernoulli_pmf = function(x) {
    0.5
  }
  X = discreteRV(f = bernoulli_pmf, type = "PMF", support = c(0, 1))

  # Test that functions evaluate to the right value
  expect_equal(getPMF(X)(0), 0.5)
  expect_equal(getPMF(X)(1), 0.5)
  expect_equal(getPMF(X)(0.5), 0)
  expect_equal(getPMF(X)(5), 0)
  expect_equal(getCDF(X)(1), 1)
  expect_equal(getCDF(X)(1), 1)
  expect_equal(getCDF(X)(0.5), 0.5)
  expect_equal(getCDF(X)(0), 0.5)
  expect_equal(getCDF(X)(1.5), 1)
  expect_equal(getCDF(X)(-1), 0)

  # Test that generating variates works
  expect_equal(length(generateVariates(X, 5)), 5)
  expect_true(all(generateVariates(X, 5) %in% c(0, 1)))

  # Test summary metrics
  expect_equal(expectedValue(X), 0.5)
  expect_equal(variance(X), 0.25)
})

test_that("Discrete random variables defined on an interval work", {
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

  # Test that functions evaluate to the right value
  expect_equal(getPMF(X)(-1), 0)
  expect_equal(getPMF(X)(0), 0)
  expect_equal(getPMF(X)(1), 0.2)
  expect_equal(getPMF(X)(2), 0.1)
  expect_equal(getPMF(X)(3), 0.5)
  expect_equal(getPMF(X)(4), 0.05)
  expect_equal(getPMF(X)(5), 0.15)
  expect_equal(getPMF(X)(6), 0)
  expect_equal(getCDF(X)(-1), 0)
  expect_equal(getCDF(X)(0), 0)
  expect_equal(getCDF(X)(1), 0.2)
  expect_equal(getCDF(X)(2), 0.3)
  expect_equal(getCDF(X)(3), 0.8)
  expect_equal(getCDF(X)(4), 0.85)
  expect_equal(getCDF(X)(5), 1)
  expect_equal(getCDF(X)(6), 1)

  # Test that generating variates works
  expect_equal(length(generateVariates(X, 5)), 5)
  expect_true(all(generateVariates(X, 5) %in% seq(1, 5)))

  # Test summary metrics
  expect_equal(expectedValue(X), 2.85)
  expect_equal(variance(X), 1.5275)
})
