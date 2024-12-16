context("LINPRED and related functions")

test_that("getTerms", {
  expect_equal(
    getTerms(~1),
    c("Intercept")
  )
  expect_equal(
    getTerms(~x+x2),
    c("Intercept", "x", "x2")
  )
  expect_equal(
    getTerms(~x*x2),
    c("Intercept", "x", "x2", "x:x2")
  )
  expect_equal(
    getTerms(~x*x2-1),
    c("x", "x2", "x:x2")
  )
  expect_equal(
    getTerms(~x*x2+0),
    c("x", "x2", "x:x2")
  )
})

test_that("getLevels", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10)) 
  expect_equal(
    getLevels(~x + x2, dat),
    list(x=c("xa","xb","xc"),
         x2=c("x2d","x2e"))
  )
  expect_equal(
    getLevels(~x3, dat),
    list(x3="x3")
  )
  expect_equal(
    getLevels(~x, dat),
    list(x=c("xa","xb","xc"))
  )
})

test_that("makeEmptyParameterStructure",{
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))

  expect_equivalent(
    makeEmptyParameterStructure(~1, dat),
    list(Intercept=array(0, dim=0))
  )
  expect_equivalent(
    makeEmptyParameterStructure(~x3, dat),
    list(Intercept=array(0, dim=0),
         x3=array(0))
  )  
  expect_equivalent(
    makeEmptyParameterStructure(~x, dat),
    list(Intercept=array(0, dim=0),
         x=array(c(xa=0,xb=0,xc=0)))
  )
  expect_equal(
    makeEmptyParameterStructure(~x:x2, dat),
    list(Intercept = structure(numeric(0), dim = 0L, dimnames = list(NULL)), 
         `x:x2` = structure(c(0, 0, 0, 0, 0, 0), dim = 3:2, 
                  dimnames = list(c("xa", "xb", "xc"), c("x2d", "x2e"))))
  )
  expect_equal(
    makeEmptyParameterStructure(~x2:x3, dat),
    list(Intercept = structure(numeric(0), dim = 0L, dimnames = list(NULL)), 
         `x2:x3` = structure(c(0, 0), dim = 2:1, 
                             dimnames = list(c("x2d", "x2e"), "x3")))
  )
})

test_that("makeParameterStructure",{
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  expect_equivalent(
    makeParameterStructure(~1, dat),
    list(Intercept=array(0, dim=0))
  )
  expect_equivalent(
    makeParameterStructure(~x3, dat),
    list(Intercept=array(0, dim=0),
         x3=1)
  )  
  expect_equivalent(
    makeParameterStructure(~x, dat),
    list(Intercept=array(0, dim=0),
         x=array(c(xa=0,xb=1,xc=1)))
  )
  expect_equal(
    makeParameterStructure(~x*x2, dat),
    list(Intercept = structure(numeric(0), dim = 0L, dimnames = list(NULL)), 
         x = structure(c(xa = 0, xb = 1, xc = 1), dim = 3L, 
                       dimnames = list(c("xa", "xb", "xc"))), 
         x2 = structure(c(x2d = 0, x2e = 1), dim = 2L, dimnames = list(c("x2d", "x2e"))),
         `x:x2` = structure(c(0, 0, 0, 0, 1, 1), dim = 3:2, 
          dimnames= list(c("xa", "xb", "xc"), c("x2d", "x2e"))))
  )
  expect_equal(
    makeParameterStructure(~x2*x3, dat),
    list(Intercept = structure(numeric(0), dim = 0L, dimnames = list(NULL)), 
         x2 = structure(c(x2d = 0, x2e = 1), dim = 2L, 
                        dimnames = list(c("x2d", "x2e"))), 
         x3 = c(x3 = 1), `x2:x3` = c(x2d = 0, x2e = 1))
  )
})

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

test_that("getFormulaBrackets", {
  expect_equal(
    getFormulaBrackets(~x + x2, list(quote(1:k))),
    c(x="[1:k]", x2="[1:k]")
  )
  expect_equal(
    getFormulaBrackets(~x[1:n] + x2, list(quote(1:k))),
    c(x="[1:n]")
  )
  expect_equal(
    getFormulaBrackets(~x[alpha[1:n]] + x2, list(quote(1:k))),
    c(x="[alpha[1:n]]")
  )
  expect_equal(
    getFormulaBrackets(~x[1:n] + x2 + x[1:n]:x2, list(quote(1:k))),
    c(x="[1:n]")
  )
})

test_that("removeDuplicateIndices",{
  expect_equal(
    removeDuplicateIndices(c(x="[1:n]")),
    c(x="[1:n]")
  )
  expect_equal(
    removeDuplicateIndices(c(x="[1:n]", x="[1:n]")),
    c(x="[1:n]")
  )
})

test_that("getVariableType",{
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  expect_equal(
    getVariableType(~x, dat),
    list(Intercept=c(Intercept="Intercept"), x=c(x="factor"))
  ) 
  expect_equal(
    getVariableType(~x+x3, dat),
    list(Intercept=c(Intercept="Intercept"), x=c(x="factor"), x3=c(x3="continuous"))
  )
  expect_equal(
    getVariableType(~x:x3, dat),
    list(Intercept=c(Intercept="Intercept"), `x:x3`=c(x="factor", x3="continuous"))
  )
  expect_equal(
    getVariableType(~x:x3-1, dat),
    list(`x:x3`=c(x="factor", x3="continuous"))
  )
})

test_that("matchVarsToBrackets", {  
  form <- ~x + x2
  bracks <- getFormulaBrackets(form, list(quote(1:n)))
  vars <- all.vars(form)
  expect_equal(
    matchVarsToBrackets(vars, bracks),
    bracks
  )
  form <- ~x[1:n] + x2
  bracks <- getFormulaBrackets(form, list(quote(1:n)))
  vars <- all.vars(form)
  expect_error(
    matchVarsToBrackets(vars, bracks)
  )
})

test_that("factorComponent", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  types <- getVariableType(~x+x2+x3+x:x2+x:x3, dat)
  bracks <- getFormulaBrackets(~x+x2+x3+x:x2+x:x3, list(quote(1:n)))

  # intercept
  expect_equal(
    factorComponent(types$Intercept, bracks),
    NULL
  )
  # continuous
  expect_equal(
    factorComponent(types$x3, bracks),
    NULL
  )
  # factor
  expect_equal(
    factorComponent(types$x, bracks),
    "[x[1:n]]"
  )
  # factor * continuous (the continuous data part gets added later)
  expect_equal(
    factorComponent(types$`x:x3`, bracks),
    "[x[1:n]]"
  )
  # factor * factor
  expect_equal(
    factorComponent(types$`x:x2`, bracks),
    "[x[1:n], x2[1:n]]"
  )
})

test_that("continuousComponent", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  types <- getVariableType(~x+x2+x3+x:x2+x:x3, dat)
  bracks <- getFormulaBrackets(~x+x2+x3+x:x2+x:x3, list(quote(1:n)))

  # intercept
  expect_equal(
    continuousComponent(types$Intercept, bracks),
    NULL
  )
  # continuous
  expect_equal(
    continuousComponent(types$x3, bracks),
    " * x3[1:n]"
  )
  # factor
  expect_equal(
    continuousComponent(types$x, bracks),
    NULL
  )
  # factor * continuous
  expect_equal(
    continuousComponent(types$`x:x3`, bracks),
    " * x3[1:n]"
  )
  # factor * factor
  expect_equal(
    continuousComponent(types$`x:x2`, bracks),
    NULL
  )
})

test_that("getParametersForLP", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  pars <- names(makeParameterStructure(~x+x2+x:x3, dat))
  
  expect_equal(
    getParametersForLP(pars, "beta_"),
    c("beta_Intercept", "beta_x", "beta_x2","beta_x_x3")
  )
})

test_that("makeLPFromFormula", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  
  expect_equal(
    makeLPFromFormula(~1, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_Intercept)
  )
  expect_equal(
    makeLPFromFormula(~x, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_Intercept + beta_x[x[1:n]])
  )
  expect_equal(
    makeLPFromFormula(~x3-1, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_x3 * x3[1:n])
  )
  expect_equal(
    makeLPFromFormula(~x:x2, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_Intercept + beta_x_x2[x[1:n], x2[1:n]])
  )
  expect_equal(
    makeLPFromFormula(~x*x3, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_Intercept + beta_x[x[1:n]] + beta_x3 * x3[1:n] + beta_x_x3[x[1:n]] * x3[1:n])
  )
  expect_equal(
    makeLPFromFormula(~x*x3 -1 -x, dat, list(quote(1:n)), quote(beta_)),
    quote(beta_x3 * x3[1:n] + beta_x_x3[x[1:n]] * x3[1:n])
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
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] + beta_x3 * x3[1:n]))
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
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] +
    beta_x2[x2[1:n]] + beta_x2_x3[x2[1:n]] * x3[1:n]))
  )

  # With modMatNames = TRUE (code should be unchanged)
  code <- quote(y[1:n] <- LINPRED(~x2, link=log, priorSpecs=NULL, modMatNames=TRUE))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    quote(log(y[1:n]) <- nimbleMacros::FORLOOP(beta_Intercept + beta_x2[x2[1:n]]))
  )

  code2 <- quote(y[1:n] <- LINPRED(~x2, link=log, priorSpecs=NULL, modMatNames=FALSE))
  expect_equal(
    LINPRED$process(code, modInfo, NULL)$code,
    LINPRED$process(code2, modInfo, NULL)$code,
  )
  
  # Covariate not in constants
  code <- quote(y[1:n] <- LINPRED(~x4, link=log, priorSpecs=NULL))
  expect_error(LINPRED$process(code, modInfo, NULL))
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

  # With subtracted intercepts
  code <- quote(y[1:n] ~ LINPRED(~-1 + x, priorSpecs=NULL))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]]))
  )

  code2 <- quote(y[1:n] ~ LINPRED(~-1 + (1|x), priorSpecs=NULL))
  out2 <- LINPRED$process(code2, modInfo, NULL)
  expect_equal(out2$code, out$code)

  code3 <- quote(y[1:n] ~ LINPRED(~-1 + (-1+x3|x), priorSpecs=NULL))
  out3 <- LINPRED$process(code3, modInfo, NULL)
  expect_equal(
    out3$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x3_x[x[1:n]] * x3[1:n]))
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
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x2[x2[1:n]] + beta_x[x[1:n]] +
      beta_x2_x[x2[1:n], x[1:n]]))
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

  code <- quote(y[1:n] ~ LINPRED(~x3 + (x3|x), priorSpecs=NULL, centerVar=x))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

  code <- quote(y[1:n] ~ LINPRED(~x3 + (x3|x) + (1|x2), priorSpecs=NULL, centerVar=x))
 
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x2[x2[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

  code <- quote(y[1:n] ~ LINPRED(~(x3|x), priorSpecs=NULL, centerVar=x))
  out <- LINPRED$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::FORLOOP(beta_x[x[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

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
    quote(mu[1:n] <- nimbleMacros::FORLOOP(beta_Intercept + beta_x[x[1:n]] + beta_x_x2[x[1:n], x2[1:n]]))
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
