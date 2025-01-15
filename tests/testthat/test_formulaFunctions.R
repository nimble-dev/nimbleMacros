context("formulaFunctions")

test_that("Error when function formula is unsupported", {
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3)))

  code <- quote(y[1:n] <- LINPRED(~test(x), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=environment()), "No processing")

  code <- quote(y[1:n] <- LINPRED(~test(x) + (1|x2), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=environment()), "No processing")

  code <- quote(y[1:n] <- LINPRED(~x3 + test(x[1:10]), priorSpecs=NULL))
  expect_error(LINPRED$process(code, modelInfo=modInfo, .env=environment()), "No processing")

})

test_that("offset formula function works", {
  nimbleOptions(enableMacroComments=FALSE)
  set.seed(123)
  constants <- list(y = rnorm(5), x = rnorm(5), z = runif(5, 0, 1), 
                  z2 = matrix(runif(5, 0, 1), 5, 1), n = 5)

  # Basic offset
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + offset(z))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )

  # Bracket
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + offset(z2[1:n,1]))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + z2[i_1, 1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )

  # Nested functions inside offset
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + offset(pnorm(log(z), 0)))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + pnorm_log_z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )
  # This should create a new constant
  expect_equal(mod$getConstants()$pnorm_log_z,
               pnorm(log(constants$z), 0))
 
  # Nested functions inside offset with bracket
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + offset(pnorm(log(z2[1:n,1]), 0)))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + pnorm_log_z2[i_1,1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )
  # This should create a new constant
  expect_equal(mod$getConstants()$pnorm_log_z2,
               pnorm(log(constants$z2), 0))

  # Error if function eval doesn't work
  code <- quote(mu[1:n] <- LINPRED(~x + offset(test(z))))
  options(show.error.messages=FALSE)
  expect_error(LINPRED$process(code, modelInfo=list(constants=constants), environment()),
               "Problem evaluating")
  options(show.error.messages=TRUE)

  nimbleOptions(enableMacroComments=TRUE)
})
