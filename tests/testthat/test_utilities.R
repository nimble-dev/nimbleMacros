context("utility functions")

skip_on_cran()

test_that("hasBracket", {
  expect_true(hasBracket(quote(beta[1])))
  expect_false(hasBracket(quote(beta)))
  expect_true(hasBracket(quote(alpha[beta[1]])))
  expect_true(hasBracket(quote(~x[1:n])))
  expect_false(hasBracket(quote(~x[1:n]), recursive=FALSE))
})

test_that("isAssignment", {
  expect_true(isAssignment(quote(alpha <- 1)))
  expect_false(isAssignment(quote(alpha(1))))
})

test_that("isAssignment", {
  expect_true(
    isAssignment(quote(alpha ~ dbern(omega)))
  )
  expect_true(
    isAssignment(quote(alpha <- beta[1]))
  )
  expect_false(
    isAssignment(quote(beta[1]))
  )
})

test_that("getLHS", {
  expect_equal(
    getLHS(quote(alpha ~ dbern(omega))),
    quote(alpha)
  )
  expect_equal(
    getLHS(quote(alpha <- beta[1])),
    quote(alpha)
  )
  expect_error(
    getLHS(quote(beta[1]))
  )
})

test_that("getRHS", {
  expect_equal(
    getRHS(quote(alpha ~ dbern(omega))),
    quote(dbern(omega))
  )
  expect_equal(
    getRHS(quote(alpha <- beta[1])),
    quote(beta[1])
  )
  expect_error(
    getRHS(quote(beta[1]))
  )
})

test_that("LHS<-", {
  code <- quote(beta[1] ~ dnorm(0, 10))
  LHS(code) <- quote(test)
  expect_equal(
    code,
    quote(test ~ dnorm(0,10))
  )
  expect_error(LHS(code) <- "test")
  code2 <- quote(alpha[1])
  expect_error(LHS(code2) <- quote(test))
})

test_that("RHS<-", {
  code <- quote(beta[1] ~ dnorm(0, 10))
  RHS(code) <- quote(test)
  expect_equal(
    code,
    quote(beta[1] ~ test)
  )
  expect_error(RHS(code) <- "test")
  code2 <- quote(alpha[1])
  expect_error(RHS(code2) <- quote(test))
})

test_that("removeExtraBrackets", {       
  # Test removing brackets inside for loops
  code <- nimbleCode({
  {
    alpha <- 1
    for (i in 1:n){
      {
        for (j in 1:k){
          z <- 1
          {
          y <- 1
          }
        }
      }
    }
  }
  })
  
  expect_equal(
    removeExtraBrackets(code),
    quote({
      alpha <- 1
      for (i in 1:n){
        for (j in 1:k){
          z <- 1
          y <- 1
        }
      }
    })
  )

})
