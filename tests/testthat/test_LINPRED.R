context("LINPRED and related functions")

test_that("removeBracketsFromFormula",{  
  expect_equal(
    removeBracketsFromFormula(~x + x2),
    ~x + x2
  )
  expect_equal(
    removeBracketsFromFormula(~x[1:n] + x2),
    ~x + x2
  )
  expect_equal(
    removeBracketsFromFormula(~x[1:n] + x2[1:m]),
    ~x + x2
  )
  expect_equal(
    removeBracketsFromFormula(~x[alpha[1:n]] + x2[1:m]),
    ~x + x2
  )
  expect_equal(
    removeBracketsFromFormula(~x[1:N, 1:J, 1:K[1:N]] + x2[1:m]),
    ~x + x2
  )
})

test_that("extractBracket", {
  expect_error(extractBracket(quote(x)))
  expect_equal(
    extractBracket(quote(x[1:n])),
    c(x="[1:n]")
  )
  expect_equal(
    extractBracket(quote(x[alpha[1:n]])),
    c(x="[alpha[1:n]]")
  )
})

test_that("extractAllBrackets", {
  expect_equal(
    extractAllBrackets(~x+x2),
    NULL
  )
  expect_equal(
    extractAllBrackets(~x[1:n]),
    c(x="[1:n]")
  )
  expect_equal(
    extractAllBrackets(~x[1:n]+x2),
    c(x="[1:n]")
  )
  expect_equal(
    extractAllBrackets(~x[1:n]+x2[1:k]),
    c(x="[1:n]", x2="[1:k]")
  )
  expect_equal(
    extractAllBrackets(~x[1:n]*x2[1:k]),
    c(x="[1:n]", x2="[1:k]")
  )
  expect_equal(
    extractAllBrackets(~x[alpha[1:n]]+x2[1:k]),
    c(x="[alpha[1:n]]", x2="[1:k]")
  )
})

test_that("makeDummyDataFrame", {
  set.seed(123)
  dat <- list(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3))
  expect_equal(
    makeDummyDataFrame(~x + x3, dat),
    data.frame(x=factor("c", levels=levels(dat$x)), x3=1.224)
  )
  expect_equal(
    makeDummyDataFrame(~x + x3 + x4, dat),
    data.frame(x=factor("c", levels=levels(dat$x)), x3=1.224, x4=0)
  )
  dat <- list(x=rnorm(3), z=NULL)
  expect_error(
    makeDummyDataFrame(~x + z, dat),
    "List element z in constants is NULL"
  )
})

test_that("LINPRED", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)),
                  inits=list(beta_Intercept=0))

  code <- quote(y[1:n] <- LINPRED(~1, priorSpecs=NULL))
 
  out <- LINPRED$process(code, modelInfo=modInfo, .env=NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept))
  )
  expect_equal(
    out$modelInfo,
    modInfo
  )

  code <- quote(y[1:n] ~ LINPRED(~x + x3, priorSpecs=NULL))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] + beta_x3 * x3[1:n]))
  )
  expect_equivalent(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
         beta_x3 = 0)
  )

  code <- quote(y[1:n] ~ LINPRED(~x, priorSpecs=NULL, coefPrefix=alpha_))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(alpha_Intercept + alpha_x[x[1:n]]))
  )

  code <- quote(y[1:n] <- LINPRED(~x, link=log, priorSpecs=NULL))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote(log(y[1:n]) <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]]))
  )

  code <- quote(y[1:n] <- LINPRED(~1, priorSpecs=setPriors()))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote({
      y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept)
      nimbleMacros::LINPRED_PRIORS(~1, coefPrefix = beta_, sdPrefix=NULL, priorSpecs=setPriors(), modMatNames=FALSE, noncenter=FALSE, centerVar=NULL)
    })
  )
  
  pr <- setPriors(sd=quote(dunif(0, 10)))
  code <- quote(y[1:n] ~ LINPRED(~1, priorSpecs=pr))
  expect_equal(
    LINPRED$process(code, modInfo, environment())$code,
    quote({
      y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept)
      nimbleMacros::LINPRED_PRIORS(~1, coefPrefix = beta_, sdPrefix=NULL, priorSpecs=pr, modMatNames=FALSE, noncenter=FALSE, centerVar=NULL)
    })
  )

  code <- quote(y[1:n] ~ LINPRED(~x + (x3|x2), priorSpecs=NULL))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] +
    beta_x2[x2[1:n]] + beta_x3_x2[x2[1:n]] * x3[1:n]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
      beta_x2 = structure(c(0, 0), dim = 2L), beta_x3_x2 = c(0, 0), sd_x2 = 1, sd_x3_x2 = 1)
  )

  # With modMatNames = TRUE (code should be unchanged)
  code <- quote(y[1:n] <- LINPRED(~x2, link=log, priorSpecs=NULL, modMatNames=TRUE))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(log(y[1:n]) <- nimbleMacros::FORLOOP(beta_Intercept + beta_x2[x2[1:n]]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x2 = structure(c(0, 0), dim = 2L))
  )

  code2 <- quote(y[1:n] <- LINPRED(~x2, link=log, priorSpecs=NULL, modMatNames=FALSE))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    LINPRED$process(code2, modInfo, NULL)$code,
  )
  
  # Covariate not in constants
  #code <- quote(y[1:n] <- LINPRED(~x4, link=log, priorSpecs=NULL))
  #expect_error(LINPRED$process(code, modInfo, NULL))
})

test_that("LINPRED with random effect", {
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3), n=10))

  code <- quote(y[1:n] ~ LINPRED(~x3 + (1|x), priorSpecs=NULL))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x3 * x3[1:n] + beta_x[x[1:n]]))
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x3 = 0, beta_x = structure(c(0, 0, 0), dim = 3L), sd_x = 1)
  )

  # With subtracted intercepts
  code <- quote(y[1:n] ~ LINPRED(~-1 + x, priorSpecs=NULL))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_x = structure(c(0, 0, 0), dim = 3L))
  )

  code2 <- quote(y[1:n] ~ LINPRED(~-1 + (1|x), priorSpecs=NULL))
  out2 <- LINPRED$process(code2, modInfo, NULL)
  expect_equal(out2$code, out$code)
  expect_equal(out2$modelInfo$inits[[1]], out$modelInfo$inits[[1]])

  code3 <- quote(y[1:n] ~ LINPRED(~-1 + (-1+x3|x), priorSpecs=NULL))
  out3 <- LINPRED$process(code3, modInfo, NULL)
  expect_equal(
    out3$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x3_x[x[1:n]] * x3[1:n]))
  )
  expect_equal(
    out3$modelInfo$inits,
    list(beta_x3_x = c(0, 0, 0), sd_x3_x = 1)
  )


  code3 <- quote(LINPRED_PRIORS(~-1 + (-1+x3|x), priorSpecs=setPriors()))
  out3 <- LINPRED_PRIORS$process(code3, modInfo, NULL)

  code4 <- nimbleCode({
    mu[1:n] ~ LINPRED(~-1 + (-1+x3|x), priorSpecs=setPriors())
  })
  mod <- nimbleModel(code4, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    "# LINPRED"
    "  ## nimbleMacros::FORLOOP"
    for (i_1 in 1:n) {
        mu[i_1] <- beta_x3_x[x[i_1]] * x3[i_1]
    }
    "  ## ----"
    "  ## nimbleMacros::LINPRED_PRIORS"
    sd_x3_x ~ dunif(0, 100)
    "    ### nimbleMacros::FORLOOP"
    for (i_2 in 1:3) {
        beta_x3_x[i_2] ~ dnorm(0, sd = sd_x3_x)
    }
    "    ### ----"
    "  ## ----"
    "# ----"
  })
  )

  # Make sure subtracting terms results in same naming pattern
  # in both LINPRED and LINPRED_PRIORS
  code5 <- quote(y[1:n] ~ LINPRED(~x*x2 - 1 -x + (1|x), priorSpecs=NULL))
  out5 <- LINPRED$process(code5, modInfo, NULL)
  expect_equal(
    out5$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x2[x2[1:n]] + beta_x_x2[x[1:n], x2[1:n]] + beta_x[x[1:n]]))
  )

  expect_equal(
    out5$modelInfo$inits,
    list(beta_x2 = structure(c(0, 0), dim = 2L), beta_x_x2 = structure(c(0, 0, 0, 0, 0, 0), dim = 3:2), 
         beta_x = structure(c(0, 0, 0), dim = 3L), sd_x = 1)
  )

  code6 <- quote(LINPRED_PRIORS(~x*x2 - 1 - x + (1|x) ))
  out6 <- LINPRED_PRIORS$process(code6, modInfo, NULL)
  expect_equal(
    out6$code,
    quote({
    beta_x2[1] ~ dnorm(0, sd = 1000)
    beta_x2[2] ~ dnorm(0, sd = 1000)
    beta_x2_x[1, 1] <- 0
    beta_x2_x[2, 1] <- 0
    beta_x2_x[1, 2] ~ dnorm(0, sd = 1000)
    beta_x2_x[2, 2] ~ dnorm(0, sd = 1000)
    beta_x2_x[1, 3] ~ dnorm(0, sd = 1000)
    beta_x2_x[2, 3] ~ dnorm(0, sd = 1000)
    sd_x ~ dunif(0, 100)
    beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    })
  )
  expect_equal(
    out6$modelInfo$inits,
    list(beta_x2 = structure(c(x2d = 0, x2e = 0), dim = 2L, dimnames = list(c("x2d", "x2e"))), 
         beta_x = structure(c(xa = 0, xb = 0, xc = 0), dim = 3L, dimnames = list(c("xa", "xb", "xc"))), 
         beta_x2_x = structure(c(0, 0, 0, 0, 0, 0), dim = 2:3, dimnames = list(c("x2d", "x2e"), c("xa", "xb", "xc"))), sd_x = 1)
  )
    
  # Generate error when trying to get random slope for factor
  code6 <- quote(y[1:n] ~ LINPRED(~ (x2|x), priorSpecs=NULL ))
  expect_error(LINPRED$process(code6, modInfo, NULL))
  code6 <- quote(LINPRED_PRIORS(~ (x2|x) ))
  expect_error(LINPRED_PRIORS$process(code6, modInfo, NULL))
})

test_that("LINPRED with 'centered' random effect", {
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] ~ LINPRED(~x3 + (1|x), priorSpecs=NULL, centerVar=x))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x3 * x3[1:n] + beta_x[x[1:n]]))
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x3 = 0, beta_x = structure(c(0, 0, 0), dim = 3L), sd_x = 1)
  )

  code2 <- quote(LINPRED_PRIORS(~x3 + (1|x), centerVar=x))
  out2 <- LINPRED_PRIORS$process(code2, modInfo, NULL)
  expect_equal(out2$modelInfo$inits, out$modelInfo$inits)
  
  code <- quote(y[1:n] ~ LINPRED(~x3 + (x3|x), priorSpecs=NULL, centerVar=x))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x3_x[x[1:n]] * x3[1:n]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x3 = 0, beta_x = structure(c(0, 0, 0), dim = 3L),
         beta_x3_x = c(0, 0, 0), sd_x = 1, sd_x3_x = 1)
  )
 
  code <- quote(y[1:n] ~ LINPRED(~x3 + (x3|x) + (1|x2), priorSpecs=NULL, centerVar=x))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x3_x[x[1:n]] * x3[1:n] + beta_x2[x2[1:n]]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x3 = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
         beta_x3_x = c(0, 0, 0), sd_x = 1, sd_x3_x = 1, 
          beta_x2 = structure(c(0, 0), dim = 2L), sd_x2 = 1)
  )
  
  code2 <- quote(LINPRED_PRIORS(~x3+(x3|x)+(1|x2), centerVar=x))
  out2 <- LINPRED_PRIORS$process(code2, modInfo,NULL)
  expect_equal(out2$modelInfo$inits, out$modelInfo$inits)

  code <- quote(y[1:n] ~ LINPRED(~(x3|x), priorSpecs=NULL, centerVar=x))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x3_x[x[1:n]] * x3[1:n]))
  )
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
      beta_x3_x = c(0, 0, 0), sd_x = 1, sd_x3_x = 1)
  )

  # Situation when centering on x and there is no mean for x3, make sure mean set to 0
  code2 <- quote(LINPRED_PRIORS(~(x3|x), centerVar=x))
  out2 <- LINPRED_PRIORS$process(code2, modInfo, NULL)

  expect_equal(
    out2$code,
    quote({
    beta_Intercept ~ dnorm(0, sd = 1000)
    sd_x ~ dunif(0, 100)
    sd_x3_x ~ dunif(0, 100)
    re_sds_x[1] <- sd_x
    re_sds_x[2] <- sd_x3_x
    Ustar_x[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
    U_x[1:2, 1:2] <- uppertri_mult_diag(Ustar_x[1:2, 1:2], re_sds_x[1:2])
    re_means_x[1] <- beta_Intercept
    re_means_x[2] <- 0
    for (i_ in 1:3) {
        B_x[i_, 1:2] ~ dmnorm(re_means_x[1:2], cholesky = U_x[1:2, 
            1:2], prec_param = 0)
        beta_x[i_] <- B_x[i_, 1]
        beta_x3_x[i_] <- B_x[i_, 2]
    }
    })
  )

  #test_code <- nimbleCode({
  #  mu[1:n] <- LINPRED(~(x3|x), centerVar=x)
  #  y[1:n] ~ FORLOOP(dnorm(mu[1:n], sd = sd_res))
  #  sd_res ~ dunif(0, 100)
  #})
  #mod <- nimbleModel(test_code, constants=c(modInfo$constants, list(n=10, y=rnorm(10))))
  #samples <- nimbleMCMC(mod, niter=10, nburnin=0)
})

test_that("LINPRED with factor array covariate", {
  set.seed(123)
  modInfo <- list(constants=list(y=matrix(rnorm(12), 3, 4),
                                 x=matrix(sample(letters[1:3], 12, replace=T), 3, 4),
                                 M=3, J=4))
  code <- quote(y[1:M,1:J] ~ LINPRED(~x[1:M,1:J], priorSpecs=NULL))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:M, 1:J] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:M, 1:J]]))
  )
  expect_equal(dim(out$modelInfo$constants$x), c(3,4))
  expect_equal(
    out$modelInfo$inits,
    list(beta_Intercept = 0,
         beta_x = structure(c(xa = 0, xb = 0, xc = 0), dim = 3L, dimnames = list(c("xa", "xb", "xc"))))
  )

  p <- nimble:::codeProcessModelMacros(code, modInfo, environment())
  expect_true(is.numeric(p$modelInfo$constants$x))
  expect_equal(dim(p$modelInfo$constants$x), c(3,4))
})

test_that("LINPRED errors when there are functions in the formula", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] <- LINPRED(~scale(x), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=NULL))

  code <- quote(y[1:n] <- LINPRED(~scale(x) + (1|x2), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=NULL))

  code <- quote(y[1:n] <- LINPRED(~x3 + I(x[1:10]), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=NULL))

})

test_that("priors macro", {
  set.seed(123)
  modInfo <- list(constants=data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10)))


  out <- nimbleMacros::LINPRED_PRIORS$process(quote(LINPRED_PRIORS(~1, coefPrefix=beta_)), modInfo, .env=NULL)  
  expect_equal(out$modelInfo$constants, modInfo$constants)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
    })
  )

  newpriors <- setPriors(intercept=quote(dnorm(0, sd=3)),
                         coefficient=quote(dnorm(0,  sd=3)))

  expect_equal(
    nimbleMacros::LINPRED_PRIORS$process(quote(LINPRED_PRIORS(~x, priorSpecs=newpriors)), modInfo, environment())$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 3)
      beta_x[1] <- 0
      beta_x[2] ~ dnorm(0, sd = 3)
      beta_x[3] ~ dnorm(0, sd = 3)
    })
  )


  expect_equal(
    nimbleMacros::LINPRED_PRIORS$process(quote(LINPRED_PRIORS(~x3, coefPrefix = alpha_)), modInfo, NULL)$code,
    quote({
      alpha_Intercept ~ dnorm(0, sd = 1000)
      alpha_x3 ~ dnorm(0, sd=1000)
    })
  )
  expect_equal(
    nimbleMacros::LINPRED_PRIORS$process(quote(LINPRED_PRIORS(~x, modMatNames=TRUE)), modInfo, NULL)$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x[1] <- 0
      beta_x[2] <- beta_xb
      beta_xb ~ dnorm(0, sd=1000)
      beta_x[3] <- beta_xc
      beta_xc ~ dnorm(0, sd=1000)
    })
  )
})

test_that("priors with random effect", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    w = factor(sample(letters[6:8], 10, replace=T)),
                    x3=round(rnorm(10),3), n=10))

  code <- quote(LINPRED_PRIORS(~x3 + (1|x)))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    })
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )
  
  # set custom prior on SD
  pr <- setPriors(sd=quote(dunif(-10,10)))
  code <- quote(LINPRED_PRIORS(~x3 + (1|x), priorSpecs=pr))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, environment())
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(-10, 10)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    })
  )

  # With subtracted intercept
  code <- quote(LINPRED_PRIORS(~-1 + x))
  out <- LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_x[1] ~ dnorm(0, sd = 1000)
      beta_x[2] ~ dnorm(0, sd = 1000)
      beta_x[3] ~ dnorm(0, sd = 1000)
    })
  )

  code2 <- quote(LINPRED_PRIORS(~-1 + (1|x)))
  out2 <- LINPRED_PRIORS$process(code2, modInfo, NULL)
  expect_equal(
    out2$code,
    quote({
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    })
  )

  code3 <- quote(LINPRED_PRIORS(~-1 + (-1+x3|x)))
  out3 <- LINPRED_PRIORS$process(code3, modInfo, NULL)
  expect_equal(
    out3$code,
    quote({
      sd_x3_x ~ dunif(0, 100)
      beta_x3_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x3_x))
    })
  )

  # Factor slope
  code4a <- quote(mu[1:n] <- LINPRED(~(x2||x), priorSpecs=NULL))
  out4a <- LINPRED$process(code4a, modInfo, NULL)
  expect_equal(
    out4a$code,
    quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] + beta_x2_x[x2[1:n], x[1:n]]))
  )

  code4b <- quote(LINPRED_PRIORS(~(x2||x)))
  out4b <- LINPRED_PRIORS$process(code4b, modInfo, NULL)
  expect_equal(
    out4b$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
      sd_x_x2d ~ dunif(0, 100)
      sd_x_x2e ~ dunif(0, 100)
      beta_x_x2[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2d))
      beta_x_x2[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2e))
    })
  )

  code5a <- quote(mu[1:n] <- LINPRED(~x3 + (x2*w||x), priorSpecs=NULL))
  out5a <- LINPRED$process(code5a, modInfo, NULL)

  expect_equal(
    out5a$code,
    quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x3 * x3[1:n] +
    beta_x[x[1:n]] + beta_x_x2[x[1:n], x2[1:n]] + beta_x_w[x[1:n],
    w[1:n]] + beta_x_x2_w[x[1:n], x2[1:n], w[1:n]]))
  )

  code5b <- quote(LINPRED_PRIORS(~x3 + (x2*w||x), priorSpecs=setPriors()))
  out5b <- LINPRED_PRIORS$process(code5b, modInfo, NULL)
  
  expect_equal(
    out5b$code,
    quote({
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    sd_x ~ dunif(0, 100)
    beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    sd_x_x2d ~ dunif(0, 100)
    sd_x_x2e ~ dunif(0, 100)
    beta_x_x2[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2d))
    beta_x_x2[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2e))
    sd_x_wf ~ dunif(0, 100)
    sd_x_wg ~ dunif(0, 100)
    sd_x_wh ~ dunif(0, 100)
    beta_x_w[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_wf))
    beta_x_w[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_wg))
    beta_x_w[1:3, 3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_wh))
    sd_x_x2d_wf ~ dunif(0, 100)
    sd_x_x2e_wf ~ dunif(0, 100)
    sd_x_x2d_wg ~ dunif(0, 100)
    sd_x_x2e_wg ~ dunif(0, 100)
    sd_x_x2d_wh ~ dunif(0, 100)
    sd_x_x2e_wh ~ dunif(0, 100)
    beta_x_x2_w[1:3, 1, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2d_wf))
    beta_x_x2_w[1:3, 2, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2e_wf))
    beta_x_x2_w[1:3, 1, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2d_wg))
    beta_x_x2_w[1:3, 2, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2e_wg))
    beta_x_x2_w[1:3, 1, 3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2d_wh))
    beta_x_x2_w[1:3, 2, 3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_x2e_wh))
    })
  )
})

test_that("priors with 'partially centered' random effect", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    w = factor(sample(letters[6:8], 10, replace=T)),
                    x3=round(rnorm(10),3), n=10))

  code <- quote(LINPRED_PRIORS(~x3 + (1|x), centerVar=x))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(beta_Intercept, sd = sd_x))
    })
  )

  code <- quote(LINPRED_PRIORS(~x3 + (1|x), centerVar=test))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x))
    })
  )

  code <- quote(LINPRED_PRIORS(~x3 + (x3||x), centerVar=x))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::FORLOOP(dnorm(beta_Intercept, sd = sd_x))
      sd_x_x3 ~ dunif(0, 100)
      beta_x_x3[1:3] ~ nimbleMacros::FORLOOP(dnorm(beta_x3, sd = sd_x_x3))
    })
  )

  code <- quote(LINPRED_PRIORS(~x3 + (x3|x), centerVar=x))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd = 1000)
      sd_x ~ dunif(0, 100)
      sd_x_x3 ~ dunif(0, 100)
      re_sds_x[1] <- sd_x
      re_sds_x[2] <- sd_x_x3
      Ustar_x[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
      U_x[1:2, 1:2] <- uppertri_mult_diag(Ustar_x[1:2, 1:2], re_sds_x[1:2])
      re_means_x[1] <- beta_Intercept
      re_means_x[2] <- beta_x3
      for (i_ in 1:3) {
        B_x[i_, 1:2] ~ dmnorm(re_means_x[1:2], cholesky= U_x[1:2,1:2], prec_param = 0)
        beta_x[i_] <- B_x[i_, 1]
        beta_x_x3[i_] <- B_x[i_, 2]
      }
    })
  )

  # Factor random slopes not supported
  code4a <- quote(mu[1:n] <- LINPRED(~x2 + (x2||x), priorSpecs=NULL, centerVar=x))
  expect_error(out4a <- LINPRED$process(code4a, modInfo, NULL))
  #expect_equal(
  #  out4a$code,
  #  quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x_x2[x[1:n], x2[1:n]]))
  #)

  code4b <- quote(LINPRED_PRIORS(~x2 + (x2||x), centerVar=x))
  expect_error(out4b <- LINPRED_PRIORS$process(code4b, modInfo, NULL))
})

test_that("priors with noncentered random effects", {

  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    w = factor(sample(letters[6:8], 10, replace=T)),
                    x3=round(rnorm(10),3), n=10))

  code <- quote(LINPRED_PRIORS(~x3 + (1|x), noncenter=TRUE))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd=1000)
      sd_x ~ dunif(0, 100)
      beta_x_raw[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x[1:3] <- nimbleMacros::FORLOOP(0 + sd_x * beta_x_raw[1:3])
    })
  )

  code <- quote(LINPRED_PRIORS(~x3 + (x3||x), noncenter=TRUE, centerVar=x))
 
  out <- nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      beta_x3 ~ dnorm(0, sd = 1000)
      sd_x ~ dunif(0, 100)
      beta_x_raw[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x[1:3] <- nimbleMacros::FORLOOP(beta_Intercept + sd_x * beta_x_raw[1:3])
      sd_x_x3 ~ dunif(0, 100)
      beta_x_x3_raw[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x_x3[1:3] <- nimbleMacros::FORLOOP(beta_x3 + sd_x_x3 * beta_x_x3_raw[1:3])
    })
  )

  code <- quote(LINPRED_PRIORS(~x3 + (x3|x), noncenter=TRUE, centerVar=x))

  # Factor slope
  code4a <- quote(mu[1:n] <- LINPRED(~(x2||x), priorSpecs=NULL, noncenter=TRUE))
  out4a <- LINPRED$process(code4a, modInfo, NULL)
  expect_equal(
    out4a$code,
    quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] + beta_x_x2[x[1:n], x2[1:n]]))
  )

  code4b <- quote(LINPRED_PRIORS(~(x2||x), noncenter=TRUE))
  out4b <- LINPRED_PRIORS$process(code4b, modInfo, NULL)
  expect_equal(
    out4b$code,
    quote({
      beta_Intercept ~ dnorm(0, sd = 1000)
      sd_x ~ dunif(0, 100)
      beta_x_raw[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x[1:3] <- nimbleMacros::FORLOOP(0 + sd_x * beta_x_raw[1:3])
      sd_x_x2d ~ dunif(0, 100)
      sd_x_x2e ~ dunif(0, 100)
      beta_x_x2_raw[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x_x2[1:3, 1] <- nimbleMacros::FORLOOP(0 + sd_x_x2d *
        beta_x_x2_raw[1:3, 1])
      beta_x_x2_raw[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
      beta_x_x2[1:3, 2] <- nimbleMacros::FORLOOP(0 + sd_x_x2e *
        beta_x_x2_raw[1:3, 2])

    })
  )
  
  code5a <- quote(mu[1:n] <- LINPRED(~x3 + (x2*w||x), noncenter=TRUE, priorSpecs=NULL))
  out5a <- LINPRED$process(code5a, modInfo, NULL)

  expect_equal(
    out5a$code,
    quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x3 * x3[1:n] +
    beta_x[x[1:n]] + beta_x_x2[x[1:n], x2[1:n]] + beta_x_w[x[1:n],
    w[1:n]] + beta_x_x2_w[x[1:n], x2[1:n], w[1:n]]))
  )

  code5b <- quote(LINPRED_PRIORS(~x3 + (x2*w||x), noncenter=TRUE, priorSpecs=setPriors()))
  out5b <- LINPRED_PRIORS$process(code5b, modInfo, NULL)
  
  expect_equal(
    out5b$code,
    quote({
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    sd_x ~ dunif(0, 100)
    beta_x_raw[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x[1:3] <- nimbleMacros::FORLOOP(0 + sd_x * beta_x_raw[1:3])
    sd_x_x2d ~ dunif(0, 100)
    sd_x_x2e ~ dunif(0, 100)
    beta_x_x2_raw[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x_x2[1:3, 1] <- nimbleMacros::FORLOOP(0 + sd_x_x2d *
        beta_x_x2_raw[1:3, 1])
    beta_x_x2_raw[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x_x2[1:3, 2] <- nimbleMacros::FORLOOP(0 + sd_x_x2e *
        beta_x_x2_raw[1:3, 2])
    sd_x_wf ~ dunif(0, 100)
    sd_x_wg ~ dunif(0, 100)
    sd_x_wh ~ dunif(0, 100)
    beta_x_w_raw[1:3, 1] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x_w[1:3, 1] <- nimbleMacros::FORLOOP(0 + sd_x_wf * beta_x_w_raw[1:3,
        1])
    beta_x_w_raw[1:3, 2] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x_w[1:3, 2] <- nimbleMacros::FORLOOP(0 + sd_x_wg * beta_x_w_raw[1:3,
        2])
    beta_x_w_raw[1:3, 3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = 1))
    beta_x_w[1:3, 3] <- nimbleMacros::FORLOOP(0 + sd_x_wh * beta_x_w_raw[1:3,
        3])
    sd_x_x2d_wf ~ dunif(0, 100)
    sd_x_x2e_wf ~ dunif(0, 100)
    sd_x_x2d_wg ~ dunif(0, 100)
    sd_x_x2e_wg ~ dunif(0, 100)
    sd_x_x2d_wh ~ dunif(0, 100)
    sd_x_x2e_wh ~ dunif(0, 100)
    beta_x_x2_w_raw[1:3, 1, 1] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 1, 1] <- nimbleMacros::FORLOOP(0 + sd_x_x2d_wf *
        beta_x_x2_w_raw[1:3, 1, 1])
    beta_x_x2_w_raw[1:3, 2, 1] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 2, 1] <- nimbleMacros::FORLOOP(0 + sd_x_x2e_wf *
        beta_x_x2_w_raw[1:3, 2, 1])
    beta_x_x2_w_raw[1:3, 1, 2] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 1, 2] <- nimbleMacros::FORLOOP(0 + sd_x_x2d_wg *
        beta_x_x2_w_raw[1:3, 1, 2])
    beta_x_x2_w_raw[1:3, 2, 2] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 2, 2] <- nimbleMacros::FORLOOP(0 + sd_x_x2e_wg *
        beta_x_x2_w_raw[1:3, 2, 2])
    beta_x_x2_w_raw[1:3, 1, 3] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 1, 3] <- nimbleMacros::FORLOOP(0 + sd_x_x2d_wh *
        beta_x_x2_w_raw[1:3, 1, 3])
    beta_x_x2_w_raw[1:3, 2, 3] ~ nimbleMacros::FORLOOP(dnorm(0,
        sd = 1))
    beta_x_x2_w[1:3, 2, 3] <- nimbleMacros::FORLOOP(0 + sd_x_x2e_wh *
        beta_x_x2_w_raw[1:3, 2, 3])
    })
  )

  # Correlated random effects don't work yet
  expect_error(nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL))
})

test_that("priors errors when there are functions in the formula", {

  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(LINPRED_PRIORS(~scale(x3) + (1|x), noncenter=TRUE))
  expect_error(nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL))
  
  code <- quote(LINPRED_PRIORS(~scale(x3) + (1|x), noncenter=TRUE)) 
  expect_error(nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL))

  code <- quote(LINPRED_PRIORS(~I(x3[1:10]), noncenter=TRUE))
  expect_error(nimbleMacros::LINPRED_PRIORS$process(code, modInfo, NULL))

})
