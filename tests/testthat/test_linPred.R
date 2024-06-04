context("linPred and related functions")

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

test_that("linPred", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] <- linPred(~1, priorSettings=NULL))
 
  out <- linPred$process(code, modelInfo=modInfo, .env=NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_Intercept))
  )
  expect_equal(
    out$modelInfo,
    modInfo
  )

  code <- quote(y[1:n] ~ linPred(~x + x3, priorSettings=NULL))
  expect_equal(
    linPred$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_Intercept + beta_x[x[1:n]] + beta_x3 * x3[1:n]))
  )

  code <- quote(y[1:n] ~ linPred(~x, priorSettings=NULL, coefPrefix=alpha_))
  expect_equal(
    linPred$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::forLoop(alpha_Intercept + alpha_x[x[1:n]]))
  )

  code <- quote(y[1:n] <- linPred(~x, link=log, priorSettings=NULL))
  expect_equal(
    linPred$process(code, modInfo, NULL)$code,
    quote(log(y[1:n]) <- nimbleMacros::forLoop(beta_Intercept + beta_x[x[1:n]]))
  )


  code <- quote(y[1:n] <- linPred(~1, priorSettings=setPriors()))
  expect_equal(
    linPred$process(code, modInfo, NULL)$code,
    quote({
      y[1:n] <- nimbleMacros::forLoop(beta_Intercept)
      nimbleMacros::priors(~1, coefPrefix = beta_, sdPrefix=NULL, priorSettings=setPriors(), modMatNames=TRUE, noncenter=FALSE, centerVar=NULL)
    })
  )
  
  pr <- setPriors(sd=quote(dunif(0, 10)))
  code <- quote(y[1:n] ~ linPred(~1, priorSettings=pr))
  expect_equal(
    linPred$process(code, modInfo, environment())$code,
    quote({
      y[1:n] <- nimbleMacros::forLoop(beta_Intercept)
      nimbleMacros::priors(~1, coefPrefix = beta_, sdPrefix=NULL, priorSettings=pr, modMatNames=TRUE, noncenter=FALSE, centerVar=NULL)
    })
  )

  code <- quote(y[1:n] ~ linPred(~x + (x|x2), priorSettings=NULL))
  expect_equal(
    linPred$process(code, modInfo, NULL)$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_Intercept + beta_x[x[1:n]] + beta_x2[x2[1:n]] + beta_x_x2[x[1:n], x2[1:n]]))
  )

})

test_that("linPred with random effect", {
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] ~ linPred(~x3 + (1|x), priorSettings=NULL))
 
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_Intercept + beta_x3 * x3[1:n] + beta_x[x[1:n]]))
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )
})

test_that("linPred with 'centered' random effect", {
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] ~ linPred(~x3 + (1|x), priorSettings=NULL, centerVar=x))
 
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_x3 * x3[1:n] + beta_x[x[1:n]]))
  )
  expect_equal(
    out$modelInfo$constants,
    modInfo$constants
  )

  code <- quote(y[1:n] ~ linPred(~x3 + (x3|x), priorSettings=NULL, centerVar=x))
 
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_x[x[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

  code <- quote(y[1:n] ~ linPred(~x3 + (x3|x) + (1|x2), priorSettings=NULL, centerVar=x))
 
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_x[x[1:n]] + beta_x2[x2[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

  code <- quote(y[1:n] ~ linPred(~(x3|x), priorSettings=NULL, centerVar=x))
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:n] <- nimbleMacros::forLoop(beta_x[x[1:n]] + beta_x_x3[x[1:n]] * x3[1:n]))
  )

})

test_that("linPred with factor array covariate", {
  set.seed(123)
  modInfo <- list(constants=list(y=matrix(rnorm(12), 3, 4),
                                 x=matrix(sample(letters[1:3], 12, replace=T), 3, 4),
                                 M=3, J=4))
  code <- quote(y[1:M,1:J] ~ linPred(~x[1:M,1:J], priorSettings=NULL))
  out <- linPred$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote(y[1:M, 1:J] <- nimbleMacros::forLoop(beta_Intercept + beta_x[x[1:M, 1:J]]))
  )
  expect_equal(dim(out$modelInfo$constants$x), c(3,4))

  p <- nimble:::codeProcessModelMacros(code, modInfo, environment())
  expect_true(is.numeric(p$modelInfo$constants$x))
  expect_equal(dim(p$modelInfo$constants$x), c(3,4))
})

test_that("linPred errors when there are functions in the formula", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] <- linPred(~scale(x), priorSettings=NULL))
  expect_error(linPred$process(code, modelInfo=modInfo, .env=NULL))

  code <- quote(y[1:n] <- linPred(~scale(x) + (1|x2), priorSettings=NULL))
  expect_error(linPred$process(code, modelInfo=modInfo, .env=NULL))

  code <- quote(y[1:n] <- linPred(~x3 + I(x[1:10]), priorSettings=NULL))
  expect_error(linPred$process(code, modelInfo=modInfo, .env=NULL))

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
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = sd_x))
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
      beta_x[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = sd_x))
    })
  )

})

test_that("priors with 'partially centered' random effect", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(priors(~x3 + (1|x), centerVar=x))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::forLoop(dnorm(beta_Intercept, sd = sd_x))
    })
  )

  code <- quote(priors(~x3 + (1|x), centerVar=test))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = sd_x))
    })
  )

  code <- quote(priors(~x3 + (x3||x), centerVar=x))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ dunif(0, 100)
      beta_x[1:3] ~ nimbleMacros::forLoop(dnorm(beta_Intercept, sd = sd_x))
      sd_x3_x ~ dunif(0, 100)
      beta_x3_x[1:3] ~ nimbleMacros::forLoop(dnorm(beta_x3, sd = sd_x3_x))
    })
  )

  code <- quote(priors(~x3 + (x3|x), centerVar=x))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd = 100)
      sd_x ~ dunif(0, 100)
      sd_x3_x ~ dunif(0, 100)
      re_sds_x[1] <- sd_x
      re_sds_x[2] <- sd_x3_x
      Ustar_x[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
      U_x[1:2, 1:2] <- uppertri_mult_diag(Ustar_x[1:2, 1:2], re_sds_x[1:2])
      re_means_x[1] <- beta_Intercept
      re_means_x[2] <- beta_x3
      for (i_ in 1:3) {
        B_x[i_, 1:2] ~ dmnorm(re_means_x[1:2], cholesky= U_x[1:2,1:2], prec_param = 0)
        beta_x[i_] <- B_x[i_, 1]
        beta_x3_x[i_] <- B_x[i_, 2]
      }
    })
  )

})

test_that("priors with noncentered random effects", {

  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(priors(~x3 + (1|x), noncenter=TRUE))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd=100)
      sd_x ~ dunif(0, 100)
      beta_x_raw[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = 1))
      beta_x[1:3] <- nimbleMacros::forLoop(0 + sd_x * beta_x_raw[1:3])
    })
  )

  code <- quote(priors(~x3 + (x3||x), noncenter=TRUE, centerVar=x))
 
  out <- nimbleMacros::priors$process(code, modInfo, NULL)
  expect_equal(
    out$code,
    quote({
      beta_Intercept ~ dunif(-100, 100)
      beta_x3 ~ dnorm(0, sd = 100)
      sd_x ~ dunif(0, 100)
      beta_x_raw[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = 1))
      beta_x[1:3] <- nimbleMacros::forLoop(beta_Intercept + sd_x * beta_x_raw[1:3])
      sd_x3_x ~ dunif(0, 100)
      beta_x3_x_raw[1:3] ~ nimbleMacros::forLoop(dnorm(0, sd = 1))
      beta_x3_x[1:3] <- nimbleMacros::forLoop(beta_x3 + sd_x3_x * beta_x3_x_raw[1:3])
    })
  )

  code <- quote(priors(~x3 + (x3|x), noncenter=TRUE, centerVar=x))
 
  # Correlated random effects don't work yet
  expect_error(nimbleMacros::priors$process(code, modInfo, NULL))
})

test_that("priors errors when there are functions in the formula", {

  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(priors(~scale(x3) + (1|x), noncenter=TRUE))
  expect_error(nimbleMacros::priors$process(code, modInfo, NULL))
  
  code <- quote(priors(~scale(x3) + (1|x), noncenter=TRUE)) 
  expect_error(nimbleMacros::priors$process(code, modInfo, NULL))

  code <- quote(priors(~I(x3[1:10]), noncenter=TRUE))
  expect_error(nimbleMacros::priors$process(code, modInfo, NULL))

})
