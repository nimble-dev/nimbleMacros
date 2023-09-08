context("priors and related functions")

test_that("setPriors",{

  # Defaults
  expect_equal(setPriors(),
               list(intercept=quote(dunif(-100,100)),
                    coefficient=quote(dnorm(0, sd = 100)),
                    sd=quote(T(dt(0, 0.01 ,1),0,)))
               )

  expect_equal(
    setPriors(factor = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(T(dt(0, 0.01 ,1),0,)),
         factor=quote(dnorm(0, 1)))
  )

  expect_equal(
    setPriors(continuous = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(T(dt(0, 0.01 ,1),0,)),
         continuous=quote(dnorm(0, 1)))
  )

  expect_equal(
    setPriors("alpha[1]" = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(T(dt(0, 0.01 ,1),0,)),
         "alpha[1]"=quote(dnorm(0, 1)))
  )
  expect_equal(
    setPriors("alpha[1]" = "dnorm(0, 1)"),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(T(dt(0, 0.01 ,1),0,)),
         "alpha[1]"=quote(dnorm(0, 1)))
  )


})

test_that("removeBracket", {
  expect_equal(
    removeBracket(quote(alpha)), quote(alpha))

  expect_equal(
    removeBracket(quote(alpha[1])), quote(alpha))

  expect_equal(
    removeBracket(quote(alpha[1,2])), quote(alpha))
})

test_that("choosePriorFromSettings", {

  priors <- setPriors(intercept = quote(dnorm(0, 1)),
                      coefficient = quote(dnorm(0, 2)),
                      continuous = quote(dnorm(0, 3)),
                      factor = quote(dnorm(0, 4)),
                      alpha = quote(dnorm(0, 5)),
                      "alpha[1]" = quote(dnorm(0, 6)))
  
  expect_equal(choosePriorFromSettings(quote(beta), "intercept", priorSettings=priors),
               quote(dnorm(0,1)))

  expect_equal(choosePriorFromSettings(quote(beta), "coefficient", priorSettings=priors),
               quote(dnorm(0,2)))

  expect_equal(choosePriorFromSettings(quote(beta), "continuous", "coefficient", priorSettings=priors),
               quote(dnorm(0,3)))

  expect_equal(choosePriorFromSettings(quote(beta), "factor", "coefficient", priorSettings=priors),
               quote(dnorm(0,4)))

  expect_equal(choosePriorFromSettings(quote(alpha), "coefficient", priorSettings=priors),
               quote(dnorm(0,5)))

  expect_equal(choosePriorFromSettings(quote(alpha[2]), "coefficient", priorSettings=priors),
               quote(dnorm(0,5)))

  expect_equal(choosePriorFromSettings(quote(alpha[1]), "coefficient", priorSettings=priors),
               quote(dnorm(0,6)))
  
  # Possible errors
  expect_error(choosePriorFromSettings(quote(beta), "fake", priorSettings=priors))

  priors$bad <- "dnorm(0, 1)"
  expect_error(choosePriorFromSettings(quote(beta), "bad", priorSettings=priors))
})

test_that("makeFixedPriorsFromFormula", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  
  priors <- setPriors(intercept = quote(dnorm(0, sd =5)),
                      coefficient = quote(dnorm(0, sd = 5)))

  expect_equal(
    makeFixedPriorsFromFormula(~1, dat, priors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 5)
    })
  )

  expect_equal(
    makeFixedPriorsFromFormula(~x3, dat, priors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 5)
      beta_x3 ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x3-1, dat, priors, quote(beta_))$code,
    quote({
      beta_x3 ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x, dat, priors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 5)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 5)
      beta_x[3] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x*x2, dat, priors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 5)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 5)
      beta_x[3] ~ dnorm(0, sd = 5)
      beta_x2[1] <- 0
      beta_x2[2] ~ dnorm(0, sd = 5)
      beta_x_x2[1, 1] <- 0
      beta_x_x2[2, 1] <- 0
      beta_x_x2[3, 1] <- 0
      beta_x_x2[1, 2] <- 0
      beta_x_x2[2, 2] ~ dnorm(0, sd = 5)
      beta_x_x2[3, 2] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x*x3, dat, priors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 5)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 5)
      beta_x[3] ~ dnorm(0, sd = 5)
      beta_x3 ~ dnorm(0, sd = 5)
      beta_x_x3[1] <- 0
      beta_x_x3[2] ~ dnorm(0, sd = 5)
      beta_x_x3[3] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x, dat, priors, quote(alpha_), modMatNames=TRUE)$code,
    quote({
      alpha_Intercept ~ dnorm(0, sd = 5)
      alpha_x[1] <- 0
      alpha_x[2] <- alpha_xb
      alpha_xb ~ dnorm(0, sd = 5)
      alpha_x[3] <- alpha_xc
      alpha_xc ~ dnorm(0, sd = 5)
    })
  )

  # Make sure prior settings are passed through
  newpriors <- setPriors(intercept = quote(dnorm(0, sd=1)),
                         continuous=quote(dnorm(0, sd=2)),
                         factor=quote(dnorm(0, sd=3)))
  expect_equal(
    makeFixedPriorsFromFormula(~x3, dat, newpriors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1)
      beta_x3 ~ dnorm(0, sd = 2)
    })
  )

  expect_equal(
    makeFixedPriorsFromFormula(~x, dat, newpriors, quote(beta_))$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 3)
      beta_x[3] ~ dnorm(0, sd = 3)
    })
  )

})

test_that("makeParameterStructureModMatNames", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  
  expect_equivalent(
    makeParameterStructureModMatNames(~x3, dat),
    list(Intercept=array(numeric(0)), x3=c(x3="x3"))
  )
  expect_equivalent(
    makeParameterStructureModMatNames(~x, dat),
    list(Intercept=array(numeric(0)), x=array(c(xa="0", xb="xb", xc="xc")))
  )
  expect_equal(
    makeParameterStructureModMatNames(~x*x2, dat),
    list(Intercept = structure(numeric(0), dim = 0L, dimnames = list(NULL)), 
         x = structure(c(xa = "0", xb = "xb", xc = "xc"), dim = 3L, 
                       dimnames= list(c("xa", "xb", "xc"))), 
         x2 = structure(c(x2d = "0", x2e = "x2e"), dim = 2L, dimnames = list(c("x2d", "x2e"))), 
         `x:x2` = structure(c("0","0", "0", "0", "xb.x2e", "xc.x2e"), dim = 3:2, 
                            dimnames = list(c("xa", "xb", "xc"), c("x2d", "x2e"))))
  )
})

test_that("priors macro", {
  set.seed(123)
  modInfo <- list(constants=data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10)))


  out <- nimbleMacros::priors$process(quote(priors(~1, coefPrefix=beta_)), modInfo, .env=NULL)  
  expect_equal(out$modelInfo$constants, modInfo$constants)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
    })
  )

  newpriors <- setPriors(intercept=quote(dnorm(0, sd=3)),
                         coefficient=quote(dnorm(0,  sd=3)))

  expect_equal(
    nimbleMacros::priors$process(quote(priors(~x, priorSettings=newpriors)), modInfo, environment())$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 3)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 3)
      beta_x[3] ~ dnorm(0, sd = 3)
    })
  )


  expect_equal(
    nimbleMacros::priors$process(quote(priors(~x3, coefPrefix = alpha_)), modInfo, NULL)$code,
    quote({
      alpha_Intercept ~ dunif(-100, 100)
      alpha_x3 ~ dnorm(0, sd=100)
    })
  )
  expect_equal(
    nimbleMacros::priors$process(quote(priors(~x, modMatNames=TRUE)), modInfo, NULL)$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x[1] <- 0
      beta_x[2] <- beta_xb
      beta_xb ~ dnorm(0, sd=100)
      beta_x[3] <- beta_xc
      beta_xc ~ dnorm(0, sd=100)
    })
  )
})

test_that("priors with random effect", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(priors(~x3 + (1|x)))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ T(dt(0,0.01,1),0,)
      beta_x[1:3] ~ forLoop(dnorm(0, sd = sd_x))
    })
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )
  
  # set custom prior on SD
  pr <- setPriors(sd=quote(dunif(-10,10)))
  code <- quote(priors(~x3 + (1|x), priorSettings=pr))
 
  out <- nimbleMacros::priors$process(code, modInfo, environment())
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ dunif(-10, 10)
      beta_x[1:3] ~ forLoop(dnorm(0, sd = sd_x))
    })
  )

})
