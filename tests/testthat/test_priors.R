context("priors and related functions")

test_that("makeFixedPriorsFromFormula", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  
  expect_equal(
    makeFixedPriorsFromFormula(~1, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.Intercept ~ dnorm(0, sd = 5)
    })
  )

  expect_equal(
    makeFixedPriorsFromFormula(~x3, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.Intercept ~ dnorm(0, sd = 5)
      beta.x3 ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x3-1, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.x3 ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.Intercept ~ dnorm(0, sd = 5)
      beta.x[1] <- 0
      beta.x[2] ~ dnorm(0, sd = 5)
      beta.x[3] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x*x2, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.Intercept ~ dnorm(0, sd = 5)
      beta.x[1] <- 0
      beta.x[2] ~ dnorm(0, sd = 5)
      beta.x[3] ~ dnorm(0, sd = 5)
      beta.x2[1] <- 0
      beta.x2[2] ~ dnorm(0, sd = 5)
      beta.x.x2[1, 1] <- 0
      beta.x.x2[2, 1] <- 0
      beta.x.x2[3, 1] <- 0
      beta.x.x2[1, 2] <- 0
      beta.x.x2[2, 2] ~ dnorm(0, sd = 5)
      beta.x.x2[3, 2] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x*x3, dat, quote(dnorm(0, sd=5)), quote(beta.)),
    quote({
      beta.Intercept ~ dnorm(0, sd = 5)
      beta.x[1] <- 0
      beta.x[2] ~ dnorm(0, sd = 5)
      beta.x[3] ~ dnorm(0, sd = 5)
      beta.x3 ~ dnorm(0, sd = 5)
      beta.x.x3[1] <- 0
      beta.x.x3[2] ~ dnorm(0, sd = 5)
      beta.x.x3[3] ~ dnorm(0, sd = 5)
    })
  )
  expect_equal(
    makeFixedPriorsFromFormula(~x, dat, quote(dnorm(0, sd=3)), quote(alpha.), modMatNames=TRUE),
    quote({
      alpha.Intercept ~ dnorm(0, sd = 3)
      alpha.x[1] <- 0
      alpha.x[2] <- alpha.xb
      alpha.xb ~ dnorm(0, sd = 3)
      alpha.x[3] <- alpha.xc
      alpha.xc ~ dnorm(0, sd = 3)
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
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))


  out <- priors$process(quote(beta. ~ priors(~1)), dat)  
  expect_equal(out$constants, dat)
  expect_equal(
    out$code,
    quote({
      beta.Intercept ~ dnorm(0, 10)
    })
  )
  expect_equal(
    priors$process(quote(beta. ~ priors(~x, coefPrior=dnorm(0, sd=3))), dat)$code,
    quote({
      beta.Intercept ~ dnorm(0, sd = 3)
      beta.x[1] <- 0
      beta.x[2] ~ dnorm(0, sd = 3)
      beta.x[3] ~ dnorm(0, sd = 3)
    })
  )
  expect_equal(
    priors$process(quote(alpha. ~ priors(~x3)), dat)$code,
    quote({
      alpha.Intercept ~ dnorm(0, 10)
      alpha.x3 ~ dnorm(0, 10)
    })
  )
  expect_equal(
    priors$process(quote(beta. ~ priors(~x, modMatNames=TRUE)), dat)$code,
    quote({
      beta.Intercept ~ dnorm(0, 10)
      beta.x[1] <- 0
      beta.x[2] <- beta.xb
      beta.xb ~ dnorm(0, 10)
      beta.x[3] <- beta.xc
      beta.xc ~ dnorm(0, 10)
    })
  )
})
