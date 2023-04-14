context("nimbleLM and related functions")

test_that("nimbleLM", {

  dat <- list(x = rnorm(3), x2 = factor(c("a","b","c")), y = rnorm(3))
  code <- quote(nimbleLM(y ~ x + x2, coefPrior=dnorm(0, sd=3), sdPrior=dunif(0, 3)))
  out <- nimbleLM$process(code, dat)

  expect_equal(
    out$code,
    quote({
      y[1:3] ~ forLoop(dnorm(mu[1:3], sd = sd.residual))
      mu[1:3] <- linPred(~x + x2, link = NULL, prefix = beta.)
      beta. ~ priors(~x + x2, sdPrefix = NULL, coefPrior = dnorm(0, sd = 3), sdPrior = dunif(0, 3), modMatNames = TRUE)
      sd.residual ~ dunif(0, 3)
    })
  )

  expect_equal(out$constants, dat)
  
  # Poisson example
  code2 <- quote(nimbleLM(y ~ x + x2, family=poisson))
  out2 <- nimbleLM$process(code2, dat)

  expect_equal(
    out2$code,
    quote({
      y[1:3] ~ forLoop(dpois(mu[1:3]))
      mu[1:3] <- linPred(~x + x2, link = log, prefix = beta.)
      beta. ~ priors(~x + x2, sdPrefix = NULL, coefPrior = dnorm(0, sd = 100), sdPrior = dunif(0, 100), modMatNames = TRUE)
    })
  )

  expect_equal(out$constants, dat)
  
  # binomial not supported yet
  code3 <- quote(nimbleLM(y ~ x + x2, family=binomial))
  expect_error(nimbleLM$process(code3, dat))
})

test_that("processFamily", {
  expect_equal(
    processFamily("poisson")$link,
    "log"
  )
  expect_equal(
    processFamily(quote(poisson))$link,
    "log"
  )
  expect_equal(
    processFamily(poisson(link='log'))$link,
    "log"
  )
  expect_equal(
    processFamily(quote(gaussian))$link,
    "identity"
  )
  expect_error(processFamily(quote(binomial)))
})

test_that("getDataDistCode", {
 
  expect_equal(
    getDataDistCode("gaussian", quote(y), quote(1:n), quote(sd)),
    quote(y ~ forLoop(dnorm(mu[1:n], sd = sd)))
  )

  expect_equal(
    getDataDistCode("poisson", quote(y), quote(1:n), quote(sd)),
    quote(y ~ forLoop(dpois(mu[1:n])))
  )

})
