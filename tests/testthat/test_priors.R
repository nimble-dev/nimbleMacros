context("setting and choosing priors")

test_that("setPriors",{

  # Defaults
  expect_equal(setPriors(),
               list(intercept=quote(dunif(-100,100)),
                    coefficient=quote(dnorm(0, sd = 100)),
                    sd=quote(dunif(0, 100)))
               )

  expect_equal(
    setPriors(factor = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(dunif(0, 100)),
         factor=quote(dnorm(0, 1)))
  )

  expect_equal(
    setPriors(continuous = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(dunif(0, 100)),
         continuous=quote(dnorm(0, 1)))
  )

  expect_equal(
    setPriors("alpha[1]" = quote(dnorm(0, 1))),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(dunif(0, 100)),
         "alpha[1]"=quote(dnorm(0, 1)))
  )
  expect_equal(
    setPriors("alpha[1]" = "dnorm(0, 1)"),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(dunif(0, 100)),
         "alpha[1]"=quote(dnorm(0, 1)))
  )
  expect_equal(
    setPriors(sd = list("dnorm", 0, sd = 3)),
    list(intercept=quote(dunif(-100,100)),
         coefficient=quote(dnorm(0, sd = 100)),
         sd=quote(dnorm(0, sd = 3)))
  )

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
