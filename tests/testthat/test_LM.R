context("LM and related functions")

test_that("LM", {

  dat <- list(x = rnorm(3), x2 = factor(c("a","b","c")), y = rnorm(3))
  modelInfo <- list(constants=dat)
  code <- quote(LM(y ~ x + x2, priorSpecs=setPriors(sd="dunif(0, 5)")))
  out <- LM$process(code, modelInfo, environment())

  expect_equal(
    out$code,
    quote({
      y[1:3] ~ FORLOOP(dnorm(mu_[1:3], sd = sd_residual))
      mu_[1:3] <- LINPRED(~x + x2, link = NULL, coefPrefix = beta_, sdPrefix = NULL,
                    priorSpecs=setPriors(sd = "dunif(0, 5)"), modMatNames=FALSE)
      sd_residual ~ dunif(0, 5)
    })
  )

  expect_equal(out$modelInfo$constants, dat)

  # With model matrix names
  code <- quote(LM(y ~ x + x2, priorSpecs=setPriors(sd="dunif(0, 5)"), modMatNames=TRUE))
  out <- LM$process(code, modelInfo, environment())

  expect_equal(
    out$code,
    quote({
      y[1:3] ~ FORLOOP(dnorm(mu_[1:3], sd = sd_residual))
      mu_[1:3] <- LINPRED(~x + x2, link = NULL, coefPrefix = beta_, sdPrefix = NULL,
                    priorSpecs=setPriors(sd = "dunif(0, 5)"), modMatNames=TRUE)
      sd_residual ~ dunif(0, 5)
    })
  )

  expect_equal(out$modelInfo$constants, dat)
  
  # Poisson example
  code2 <- quote(LM(y ~ x + x2, family=poisson))
  out2 <- LM$process(code2, modelInfo)

  expect_equal(
    out2$code,
    quote({
      y[1:3] ~ FORLOOP(dpois(mu_[1:3]))
      mu_[1:3] <- LINPRED(~x + x2, link = log, coefPrefix = beta_,
                    sdPrefix = NULL, priorSpecs = setPriors(), modMatNames=FALSE)
    })
  )

  expect_equal(out$modelInfo$constants, dat)
  
  # Binomial example (aggregated)
  constants <- list(y=c(1,2,3), ny = c(0,1,3), x=rnorm(3))
  modelInfo <- list(constants=constants)
  code3 <- quote(LM(cbind(y, ny) ~ x, family=binomial))
  out3 <- LM$process(code3, modelInfo)
  expect_equal(
    out3$code,
    quote({
      y[1:3] ~ FORLOOP(dbinom(mu_[1:3], size = binSize[1:3]))
      mu_[1:3] <- LINPRED(~x, link = logit, coefPrefix = beta_,
          sdPrefix = NULL, priorSpecs = setPriors(), modMatNames=FALSE)
    })
  )
  
  # Make sure sample size was calculated and added
  expect_equal(out3$modelInfo$constants$binSize,
               out3$modelInfo$constants$y + out3$modelInfo$constants$ny)

  # Binomial example (binary)
  modelInfo$constants <- list(y=c(0,1,0), x=rnorm(3))
  code4 <- quote(LM(y ~ x, family=binomial))
  out4 <- LM$process(code4, modelInfo)
  expect_equal(
    out4$code,
    quote({
      y[1:3] ~ FORLOOP(dbinom(mu_[1:3], size = 1))
      mu_[1:3] <- LINPRED(~x, link = logit, coefPrefix = beta_,
          sdPrefix = NULL, priorSpecs = setPriors(), modMatNames=FALSE)
    })
  )
  expect_true(is.null(out4$modelInfo$constants$binSize))
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
})

test_that("getDataDistCode", {
 
  expect_equal(
    getDataDistCode("gaussian", quote(y), quote(1:n), quote(sd)),
    quote(y ~ FORLOOP(dnorm(mu_[1:n], sd = sd)))
  )

  expect_equal(
    getDataDistCode("poisson", quote(y), quote(1:n), quote(sd)),
    quote(y ~ FORLOOP(dpois(mu_[1:n])))
  )

  expect_error(
    getDataDistCode("Gamma", quote(y), quote(1:n))
  )

})

test_that("Missing data error trap", {
  modelInfo <- list(constants=  list(x = rnorm(3), x2 = factor(c("a","b","c"))))

  code <- quote(LM(y ~ x + x2))
  expect_error(LM$process(code, modelInfo, environment()), "dimensions for")
})
