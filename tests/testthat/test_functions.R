test_that("Mapping random variables works", {
  # Create a normal random variable and then apply a transformation
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = createRV(f = normal_density, type = "PDF")
  g = function(x) return(x^3 + 1)
  Y = mapRV(X, g)
  expect_is(Y, "RV")
})

test_that("Convolutions work", {
  normal_density = function(x) {
    dnorm(x, mean = 0, sd = 1)
  }
  X = createRV(f = normal_density, type = "PDF")
  Y = createRV(f = normal_density, type = "PDF")
  Z = X %convolution% Y
  expect_is(Z, "RV")
})
