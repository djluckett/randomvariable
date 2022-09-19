test_that("Mapping random variables works", {
  # Create a normal random variable and then apply a transformation
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  g = function(x) return(x^3 + 1)
  Y = mapRV(X, g)
  expect_is(Y, "RV")
})

test_that("Convolutions work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %convolution% Y
  expect_is(Z, "RV")
  f = getPDF(Z)
  expect_is(f(1), "numeric")
})

test_that("Products work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %product% Y
  expect_is(Z, "RV")
  f = getPDF(Z)
  expect_is(f(1), "numeric")
})

test_that("Differences work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %difference% Y
  expect_is(Z, "RV")
  f = getPDF(Z)
  expect_is(f(1), "numeric")
})

test_that("Quotients work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = RV(f = normal_density, type = "PDF")
  Y = RV(f = normal_density, type = "PDF")
  Z = X %quotient% Y
  expect_is(Z, "RV")
  f = getPDF(Z)
  expect_is(f(1), "numeric")
})
