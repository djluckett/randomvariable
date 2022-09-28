test_that("We can create a normal random variable", {
  # Test the RV class by defining a normal random variable
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")

  # Test that mean and variance are as expected
  expect_equal(expectedValue(X), 0)
  expect_equal(variance(X), 1)

  # Test that functions evaluate to the right value
  expect_equal(getCDF(X)(0), 0.5)
  expect_equal(getCDF(X)(1.96), 0.975, tolerance = 0.001)
  expect_equal(getInverseCDF(X)(0.5), 0)
  expect_equal(getInverseCDF(X)(0.975), 1.96, tolerance = 0.001)
  expect_equal(getMGF(X)(0), 1)
  expect_gt(getPDF(X)(0.5) - getPDF(X)(1), 0)
  expect_equal(getSF(X)(0), 0.5)

  # Test that random variates are the correct class and length
  expect_is(generateVariates(X, 1), "numeric")
  expect_is(generateVariates(X, 5), "numeric")
  expect_equal(length(generateVariates(X, 3)), 3)
})

test_that("Random variable functions are vectorized", {
  # Create a normal random variable
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")

  # Verify that each function produces the correct length output
  expect_equal(length(getPDF(X)(c(1, 2, 3))), 3)
  expect_equal(length(getCDF(X)(c(1, 2, 3))), 3)
  expect_equal(length(getInverseCDF(X)(c(0.1, 0.2, 0.3))), 3)
  expect_equal(length(getSF(X)(c(0.1, 0.2, 0.3))), 3)
  expect_equal(length(getHF(X)(c(1, 2, 3))), 3)
  expect_equal(length(getCHF(X)(c(1, 2, 3))), 3)
  expect_equal(length(getMGF(X)(c(0.1, 0.2, 0.3))), 3)
})
