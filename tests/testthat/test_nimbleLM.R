context("nimbleLM and related functions")

test_that("nimbleLM", {

  dat <- list(x = rnorm(3), x2 = factor(c("a","b","c")), y = rnorm(3))
  modelInfo <- list(constants=dat)
  code <- quote(nimbleLM(y ~ x + x2, priorSettings=setPriors(sd=dunif(0, 5))))
  out <- nimbleLM$process(code, modelInfo)

  expect_equal(
    out$code,
    quote({
      y[1:3] ~ forLoop(dnorm(mu_[1:3], sd = sd_residual))
      mu_[1:3] <- linPred(~x + x2, link = NULL, coefPrefix = beta_)
      priors(~x + x2, coefPrefix = beta_, sdPrefix = NULL, priorSettings=setPriors(sd = dunif(0, 5)), modMatNames = TRUE)
      sd_residual ~ dunif(0, 5)
    })
  )

  expect_equal(out$modelInfo$constants, dat)
  
  # Poisson example
  code2 <- quote(nimbleLM(y ~ x + x2, family=poisson))
  out2 <- nimbleLM$process(code2, modelInfo)

  expect_equal(
    out2$code,
    quote({
      y[1:3] ~ forLoop(dpois(mu_[1:3]))
      mu_[1:3] <- linPred(~x + x2, link = log, coefPrefix = beta_)
      priors(~x + x2, coefPrefix = beta_, sdPrefix = NULL, priorSettings = setPriors(), modMatNames = TRUE)
    })
  )

  expect_equal(out$modelInfo$constants, dat)
  
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
    quote(y ~ forLoop(dnorm(mu_[1:n], sd = sd)))
  )

  expect_equal(
    getDataDistCode("poisson", quote(y), quote(1:n), quote(sd)),
    quote(y ~ forLoop(dpois(mu_[1:n])))
  )

})
