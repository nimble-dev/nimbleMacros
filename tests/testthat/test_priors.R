context("setting and choosing priors")

skip_on_cran()

test_that("setPriors",{

  # Defaults
  expect_equal(setPriors(),
                list(intercept=quote(dnorm(0, sd = 1000)),
                    coefficient=quote(dnorm(0, sd = 1000)),
                    sd=quote(dunif(0, 100)), lkjShape=1)
               )

  expect_equal(
    setPriors(factor = quote(dnorm(0, 1))),
    list(intercept=quote(dnorm(0, sd = 1000)),
         coefficient=quote(dnorm(0, sd = 1000)),
         sd=quote(dunif(0, 100)),
         factor=quote(dnorm(0, 1)), lkjShape=1)
  )

  expect_equal(
    setPriors(continuous = quote(dnorm(0, 1))),
    list(intercept=quote(dnorm(0, sd = 1000)),
         coefficient=quote(dnorm(0, sd = 1000)),
         sd=quote(dunif(0, 100)),
         continuous=quote(dnorm(0, 1)), lkjShape=1)
  )

  expect_equal(
    setPriors("alpha[1]" = quote(dnorm(0, 1))),
    list(intercept=quote(dnorm(0, sd = 1000)),
         coefficient=quote(dnorm(0, sd = 1000)),
         sd=quote(dunif(0, 100)),
         "alpha[1]"=quote(dnorm(0, 1)), lkjShape=1)
  )
  expect_equal(
    setPriors("alpha[1]" = "dnorm(0, 1)"),
    list(intercept=quote(dnorm(0, sd = 1000)),
         coefficient=quote(dnorm(0, sd = 1000)),
         sd=quote(dunif(0, 100)),
         "alpha[1]"=quote(dnorm(0, 1)), lkjShape=1)
  )
  expect_equal(
    setPriors(sd = list("dnorm", 0, sd = 3)),
    list(intercept=quote(dnorm(0, sd = 1000)),
         coefficient=quote(dnorm(0, sd = 1000)),
         sd=quote(dnorm(0, sd = 3)), lkjShape=1)
  )

  # Prior not quoted should error
  expect_error(expect_warning(
    setPriors(intercept = dunif(0, 1))
  ))

})

test_that("convertListToPrior", {
  
  expect_equal(
    convertListToPrior(list("dunif", 0, 10)),
    quote(dunif(0, 10))
  )
  expect_equal(
    convertListToPrior(list(quote(dnorm), 0, sd = 10)),
    quote(dnorm(0, sd = 10))
  )
  expect_error(convertListToPrior(list(dnorm, 0, sd = 10)))

})

test_that("removeBracket", {
  expect_equal(
    removeBracket(quote(alpha)), quote(alpha))

  expect_equal(
    removeBracket(quote(alpha[1])), quote(alpha))

  expect_equal(
    removeBracket(quote(alpha[1,2])), quote(alpha))
})

test_that("matchPrior", {

  priors <- setPriors(intercept = quote(dnorm(0, 1)),
                      coefficient = quote(dnorm(0, 2)),
                      continuous = quote(dnorm(0, 3)),
                      factor = quote(dnorm(0, 4)),
                      alpha = quote(dnorm(0, 5)),
                      "alpha[1]" = quote(dnorm(0, 6)))
  
  expect_equal(matchPrior(quote(beta), "intercept", priors=priors),
               quote(dnorm(0,1)))

  expect_equal(matchPrior(quote(beta), "coefficient", priors=priors),
               quote(dnorm(0,2)))

  expect_equal(matchPrior(quote(beta), "continuous", "coefficient", priors=priors),
               quote(dnorm(0,3)))

  expect_equal(matchPrior(quote(beta), "factor", "coefficient", priors=priors),
               quote(dnorm(0,4)))

  expect_equal(matchPrior(quote(alpha), "coefficient", priors=priors),
               quote(dnorm(0,5)))

  expect_equal(matchPrior(quote(alpha[2]), "coefficient", priors=priors),
               quote(dnorm(0,5)))

  expect_equal(matchPrior(quote(alpha[1]), "coefficient", priors=priors),
               quote(dnorm(0,6)))
  
  # Possible errors
  expect_error(matchPrior(quote(beta), "fake", priors=priors))

  priors$bad <- "dnorm(0, 1)"
  expect_error(matchPrior(quote(beta), "bad", priors=priors))
})

test_that("spaces in factor levels are handled", {
  y <- rnorm(3)
  x <- rnorm(3)
  z <- factor(c("lev1", "lev 2", "lev3"), levels=c("lev1","lev 2", "lev3"))

  dat <- list(y=y, x=x, z=z, n=3)

  code <- nimbleCode({
    LINPRED_PRIORS(~x + z, modelMatrixNames=TRUE) 
  })
  
  nimbleOptions(enableMacroComments=FALSE)
  mod <- nimbleModel(code, constants=dat)
  expect_equal(mod$getCode()[[5]],
               quote(beta_z[2] <- beta_zlev_2))
})
