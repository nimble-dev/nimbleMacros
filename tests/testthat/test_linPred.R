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
    getParametersForLP(pars, "beta."),
    c("beta.Intercept", "beta.x", "beta.x2","beta.x.x3")
  )
})

test_that("makeLPFromFormula", {
  set.seed(123)
  dat <- data.frame(x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=rnorm(10))
  
  expect_equal(
    makeLPFromFormula(~1, dat, list(quote(1:n)), quote(beta.)),
    quote(beta.Intercept)
  )
  expect_equal(
    makeLPFromFormula(~x, dat, list(quote(1:n)), quote(beta.)),
    quote(beta.Intercept + beta.x[x[1:n]])
  )
  expect_equal(
    makeLPFromFormula(~x3-1, dat, list(quote(1:n)), quote(beta.)),
    quote(beta.x3 * x3[1:n])
  )
  expect_equal(
    makeLPFromFormula(~x:x2, dat, list(quote(1:n)), quote(beta.)),
    quote(beta.Intercept + beta.x.x2[x[1:n], x2[1:n]])
  )
  expect_equal(
    makeLPFromFormula(~x*x3, dat, list(quote(1:n)), quote(beta.)),
    quote(beta.Intercept + beta.x[x[1:n]] + beta.x3 * x3[1:n] + beta.x.x3[x[1:n]] * x3[1:n])
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
})

test_that("linPred", {
  set.seed(123)
  dat <- list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3))

  code <- quote(y[1:n] ~ linPred(~1))
 
  out <- linPred$process(code, dat)
  expect_equal(
    out$code,
    quote(y[1:n] ~ forLoop(beta.Intercept))
  )
  expect_equal(
    out$constants,
    dat
  )

  code <- quote(y[1:n] ~ linPred(~x + x3))
  expect_equal(
    linPred$process(code, dat)$code,
    quote(y[1:n] ~ forLoop(beta.Intercept + beta.x[x[1:n]] + beta.x3 * x3[1:n]))
  )

  code <- quote(y[1:n] ~ linPred(~x, coefPrefix=alpha.))
  expect_equal(
    linPred$process(code, dat)$code,
    quote(y[1:n] ~ forLoop(alpha.Intercept + alpha.x[x[1:n]]))
  )

  code <- quote(y[1:n] ~ linPred(~x, link=log))
  expect_equal(
    linPred$process(code, dat)$code,
    quote(log(y[1:n]) ~ forLoop(beta.Intercept + beta.x[x[1:n]]))
  )

  code <- quote(y[1:n] ~ linPred(~1, coefPrior=dnorm(0, sd=5)))
  expect_equal(
    linPred$process(code, dat)$code,
    quote({
      y[1:n] ~ forLoop(beta.Intercept)
      beta. ~ priors(~1, coefPrior = dnorm(0, sd=5), sdPrefix=NULL, sdPrior=T(dt(0,0.1,1), 0, ), modMatNames=TRUE)
    })
  )

  code <- quote(y[1:n] ~ linPred(~1, sdPrior=dunif(0, 10)))
  expect_equal(
    linPred$process(code, dat)$code,
    quote({
      y[1:n] ~ forLoop(beta.Intercept)
      beta. ~ priors(~1, coefPrior = dnorm(0, sd=10), sdPrefix=NULL, sdPrior=dunif(0, 10), modMatNames=TRUE)
    })
  )

  code <- quote(y[1:n] ~ linPred(~x + (x|x2)))
  expect_equal(
    linPred$process(code, dat)$code,
    quote(y[1:n] ~ forLoop(beta.Intercept + beta.x[x[1:n]] + beta.x2[x2[1:n]] + beta.x.x2[x[1:n], x2[1:n]]))
  )

})
