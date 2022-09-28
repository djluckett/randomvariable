test_that("Mapping random variables works", {
  # Create a normal random variable and then apply a transformation
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  g = function(x) return(x^3 + 1)
  Y = mapRV(X, g)

  # Check that the result is another random variable
  expect_is(Y, "RV")

  # Check that we can evaluate standard functions for Y
  expect_is(getPDF(Y)(1), "numeric")
  expect_is(getCDF(Y)(1), "numeric")
})

test_that("Convolutions work", {
  # Create two normal random variables and add them
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %convolution% Y

  # Check that the result is another random variable
  expect_is(Z, "RV")

  # Check that we can evaluate standard functions for Z
  expect_is(getPDF(Z)(1), "numeric")
  expect_is(getCDF(Z)(1), "numeric")
})

test_that("Products work", {
  # Create two normal random variables and multiply them
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %product% Y

  # Check that the result is another random variable
  expect_is(Z, "RV")

  # Check that we can evaluate standard functions for Z
  expect_is(getPDF(Z)(1), "numeric")
  expect_is(getCDF(Z)(1), "numeric")
})

test_that("Differences work", {
  # Create two normal random variables and multiply them
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %difference% Y

  # Check that the result is another random variable
  expect_is(Z, "RV")

  # Check that we can evaluate standard functions for Z
  expect_is(getPDF(Z)(1), "numeric")
  expect_is(getCDF(Z)(1), "numeric")
})

test_that("Quotients work", {
  # Create two normal random variables and divide them
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %quotient% Y

  # Check that the result is another random variable
  expect_is(Z, "RV")

  # Check that we can evaluate standard functions for Z
  expect_is(getPDF(Z)(1), "numeric")
  expect_is(getCDF(Z)(1), "numeric")
})
