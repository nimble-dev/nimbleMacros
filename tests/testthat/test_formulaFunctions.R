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

test_that("scale formula function works", {
  nimbleOptions(enableMacroComments=FALSE)
  set.seed(123)
  constants <- list(y=rnorm(3), x = runif(3, 5, 10), z = rnorm(3), n=3, 
                    x2=matrix(runif(3,5,10), 3, 1), z2=matrix(rnorm(3), 3, 1))

  # Basic scale
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x) + z) 
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_scaled * x_scaled[i_1] + 
            beta_z * z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_scaled ~ dnorm(0, sd = 1000)
    beta_z ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_scaled,
    scale(constants$x)
  )
  # x is not removed from constants
  expect_equal(
    mod$getConstants()$x,
    constants$x
  )

  # Interaction with non-scaled term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x):z) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_scaled_z * x_scaled[i_1] * 
            z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_scaled_z ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_scaled,
    scale(constants$x)
  )

  # Interaction and linear term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x) + scale(x):z) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_scaled * x_scaled[i_1] + 
            beta_x_scaled_z * x_scaled[i_1] * z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_scaled ~ dnorm(0, sd = 1000)
    beta_x_scaled_z ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_scaled,
    scale(constants$x)
  )

  # Non-scaled term comes first in interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~z:scale(x)) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_z_x_scaled * z[i_1] * 
            x_scaled[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_z_x_scaled ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_scaled,
    scale(constants$x)
  )

  # Interaction of two scaled terms
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x):scale(z)) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_scaled_z_scaled * 
            x_scaled[i_1] * z_scaled[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_scaled_z_scaled ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_scaled,
    scale(constants$x)
  )
  expect_equivalent(
    mod$getConstants()$z_scaled,
    scale(constants$z)
  )

  # Scale with brackets
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x2[1:n,1]):z)
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2_scaled_z * x2_scaled[i_1, 
            1] * z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2_scaled_z ~ dnorm(0, sd = 1000)
  })
  )
  expect_equivalent(
    mod$getConstants()$x2_scaled,
    scale(constants$x2)
  )
  expect_equal(dim(mod$getConstants()$x2_scaled), dim(constants$x2))

  # Scale with brackets on the other interaction term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~scale(x2[1:n,1]):z2[1:n,1])
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2_scaled_z2 * x2_scaled[i_1, 
            1] * z2[i_1, 1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2_scaled_z2 ~ dnorm(0, sd = 1000)
  })
  )

  # Error if covariate is not in constants
  code <- quote(mu[1:n] <- LINPRED(~scale(test)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "Covariate inside"
  )

  # Error if expression inside scale
  code <- quote(mu[1:n] <- LINPRED(~scale(x*x)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "expression"
  )

  # Error if multiple types of functions in an interaction
  code <- quote(mu[1:n] <- LINPRED(~scale(x):test(z)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "multiple different formula functions"
  )

  nimbleOptions(enableMacroComments=TRUE)
})
