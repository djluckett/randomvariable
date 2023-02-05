test_that("We can create a Bernoulli random variable", {
  # Test the RV class by defining a normal random variable
  bernoulli_pmf = function(x) {
    0.5
  }
  X = discreteRV(f = bernoulli_pmf, type = "PMF", support = c(0, 1))

  # Test that functions evaluate to the right value
  expect_equal(getPMF(X)(0), 0.5)
  expect_equal(getPMF(X)(1), 0.5)
  expect_equal(getPMF(X)(0.5), 0)
  expect_equal(getPMF(X)(5), 0)
})
