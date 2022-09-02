test_that("We can create a normal random variable", {
  # Test the RV class by defining a normal random variable
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = createRV(f = normal_density, type = "PDF")
  expect_equal(expectedValue(X), 0)
  expect_equal(variance(X), 1)
  expect_equal(getInverseCDF(X)(0.5), 0)
  expect_equal(getInverseCDF(X)(0.975), 1.96, tolerance = 0.001)
  expect_equal(getCDF(X)(0), 0.5)
  expect_equal(getCDF(X)(1.96), 0.975, tolerance = 0.001)
  expect_is(generateVariates(X, 1), "numeric")
  expect_is(generateVariates(X, 5), "numeric")
  expect_gt(getPDF(X)(0.5) - getPDF(X)(1), 0)
})

test_that("Random variables functions are vectorized", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = createRV(f = normal_density, type = "PDF")
  expect_equal(length(getPDF(X)(c(1, 2, 3))), 3)

})

test_that("Moment generating functions work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = createRV(f = normal_density, type = "PDF")
  m = getMGF(X)
  expect_equal(m(0), 1)
})
