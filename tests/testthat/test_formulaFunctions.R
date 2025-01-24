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
                    x2=matrix(runif(3,5,10), 3, 1), z2=matrix(rnorm(3), 3, 1),
                    x3 = matrix(runif(9,5,10), 3, 3))

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

  # Scale with non-vectors
  code <- nimbleCode({
    mu[1:n,1:3] <- LINPRED(~scale(x3[1:n,1:3])) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        for (i_2 in 1:3) {
            mu[i_1, i_2] <- beta_Intercept + beta_x3_scaled * 
                x3_scaled[i_1, i_2]
        }
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3_scaled ~ dnorm(0, sd = 1000)
    })
  )
  x3_scale <- mod$getConstants()$x3_scale
  expect_equal(dim(x3_scale), dim(constants$x3))
  expect_equal(mean(x3_scale), 0, tol=1e-6)
  expect_equal(sd(x3_scale), 1, tol=1e-6)

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

test_that("I() formula function works", {
  nimbleOptions(enableMacroComments=FALSE)
  set.seed(123)
  constants <- list(x=rnorm(3), z=rnorm(3), a=matrix(rnorm(3), 3, 1), n=3)

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + I(z^2))
  })
  mod <- nimbleModel(code, constants=constants)
  
  # Basic example
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_z_2 * 
            z_2[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_z_2 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$getConstants()$z_2,
    constants$z^2
  )

  # Involved in interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x:I(z^2))
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_z_2 * x[i_1] * z_2[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_z_2 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$getConstants()$z_2,
    constants$z^2
  )

  # Subtraction operation
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~I(x-1))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_1 * x_1[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_1 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$getConstants()$x_1,
    constants$x - 1
  )

  # Indices
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + I(a[1:n,1]^2))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_a_2 * 
            a_2[i_1, 1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_a_2 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$getConstants()$a_2,
    constants$a^2
  )

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + z:I(a[1:n,1]^2))
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_z_a_2 * 
            z[i_1] * a_2[i_1, 1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_z_a_2 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$getConstants()$a_2,
    constants$a^2
  )

  # Two variables in I() not allowed
  code <- quote(mu[1:n] <- LINPRED(~I(x+z)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "more than one"
  )

  nimbleOptions(enableMacroComments=TRUE)
})

test_that("log formula function works", {
  nimbleOptions(enableMacroComments=FALSE)
  set.seed(123)
  constants <- list(y=rnorm(3), x = runif(3, 5, 10), z = rnorm(3), n=3, 
                    x2=matrix(runif(3,5,10), 3, 1), z2=matrix(rnorm(3), 3, 1),
                    x3 = matrix(runif(9,5,10), 3, 3))

  # Basic log
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~log(x) + z) 
  })
  mod <- nimbleModel(code, constants=constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_log * x_log[i_1] + 
            beta_z * z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_log ~ dnorm(0, sd = 1000)
    beta_z ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_log,
    log(constants$x)
  )

  # log with non-vectors
  code <- nimbleCode({
    mu[1:n,1:3] <- LINPRED(~log(x3[1:n,1:3])) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        for (i_2 in 1:3) {
            mu[i_1, i_2] <- beta_Intercept + beta_x3_log * 
                x3_log[i_1, i_2]
        }
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3_log ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(mod$getConstants()$x3_log, log(constants$x3))

  # Interaction with non-scaled term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~log(x):z) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_log_z * x_log[i_1] * 
            z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_log_z ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_log,
    log(constants$x)
  )

  # Non-log term comes first in interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~z:log(x)) 
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_z_x_log * z[i_1] * 
            x_log[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_z_x_log ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_log,
    log(constants$x)
  )

  # Interaction of two logged terms
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~log(x):log(z)) 
  })
  expect_warning(mod <- nimbleModel(code, constants=constants), "NaN")
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x_log_z_log * 
            x_log[i_1] * z_log[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x_log_z_log ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(
    mod$getConstants()$x_log,
    log(constants$x)
  )
  expect_equivalent(
    mod$getConstants()$z_log,
    expect_warning(log(constants$z))
  )

  # log with brackets
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~log(x2[1:n,1]):z)
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2_log_z * x2_log[i_1, 
            1] * z[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2_log_z ~ dnorm(0, sd = 1000)
  })
  )
  expect_equivalent(
    mod$getConstants()$x2_log,
    log(constants$x2)
  )

  # Scale with brackets on the other interaction term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~log(x2[1:n,1]):z2[1:n,1])
  })
  mod <- nimbleModel(code, constants=constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2_log_z2 * x2_log[i_1, 
            1] * z2[i_1, 1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2_log_z2 ~ dnorm(0, sd = 1000)
  })
  )

  # Error if covariate is not in constants
  code <- quote(mu[1:n] <- LINPRED(~log(test)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "Covariate inside"
  )

  # Error if expression inside scale
  code <- quote(mu[1:n] <- LINPRED(~log(x*x)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "expression"
  )

  # Error if multiple types of functions in an interaction
  code <- quote(mu[1:n] <- LINPRED(~log(x):scale(z)))
  expect_error(
    LINPRED$process(code, list(constants=constants), environment()),
    "multiple different formula functions"
  )

  nimbleOptions(enableMacroComments=TRUE)
})
