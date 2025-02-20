context("LINPRED and related functions")

skip_on_cran()

test_that("LINPRED basic fixed effects models", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x3=round(rnorm(10),3), x4=round(rnorm(10), 3), n = 10),
                  inits=list(beta_Intercept=0))

  # Intercept-only model
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~1)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    })
  )

  new_const <- modInfo$constants
  new_const$x <- as.numeric(new_const$x)
  new_const$x2 <- as.numeric(new_const$x2)
  expect_equal(mod$getConstants(), new_const)
  expect_equal(mod$modelDef$macroInits, list(beta_Intercept = 0))

  # Covariates both factor and continous
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + x3)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x3 * 
            x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  expect_equivalent(mod$modelDef$macroInits, list(beta_Intercept = 0, 
                                                  beta_x= structure(c(0, 0, 0), dim = 3L),
                                                  beta_x3=0))
  expect_equivalent(mod$beta_x3, 0)

  # Drop intercept
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + x3 - 1)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_x[x[i_1]] + beta_x3 * x3[i_1]
    }
    beta_x[1] ~ dnorm(0, sd = 1000)
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Change prefix
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x, coefPrefix=alpha_)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({    
    for (i_1 in 1:n) {
        mu[i_1] <- alpha_Intercept + alpha_x[x[i_1]]
    }
    alpha_Intercept ~ dnorm(0, sd = 1000)
    alpha_x[1] <- 0
    alpha_x[2] ~ dnorm(0, sd = 1000)
    alpha_x[3] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(mod$modelDef$macroInits, list(alpha_Intercept = 0, 
                                                  alpha_x = structure(c(0, 0, 0), dim = 3L)))

  # With link function
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3, link = log)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        log(mu[i_1]) <- beta_Intercept + beta_x3 * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Set custom priors
  priors <- setPriors(intercept="dnorm(0, sd=1)", coefficient="dnorm(0, sd=2)")
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3, priorSpecs=priors)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x3 * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1)
    beta_x3 ~ dnorm(0, sd = 2)
    })
  )

  # No priors
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3, priorSpecs=NULL)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x3 * x3[i_1]
    }
    })
  )

  # Modmatnames = TRUE
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + x3, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
      for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x3 * 
            x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] <- beta_xb
    beta_xb ~ dnorm(0, sd = 1000)
    beta_x[3] <- beta_xc
    beta_xc ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Continous-continous interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3*x4)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x3 * x3[i_1] + beta_x4 * 
            x4[i_1] + beta_x3_x4 * x3[i_1] * x4[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x4 ~ dnorm(0, sd = 1000)
    beta_x3_x4 ~ dnorm(0, sd = 1000)
    })
  )
  expect_equivalent(mod$modelDef$macroInits, 
                    list(beta_Intercept=0, beta_x3=0, beta_x4=0, beta_x3_x4=0))


  # Continous-factor interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x3)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x3 * 
            x3[i_1] + beta_x_x3[x[i_1]] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[1] <- 0
    beta_x_x3[2] ~ dnorm(0, sd = 1000)
    beta_x_x3[3] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
    beta_x3 = 0, beta_x_x3 = c(0, 0, 0))
  )

  # continuous-factor interaction with modelMatrixNames = TRUE
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x3, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x3 * 
            x3[i_1] + beta_x_x3[x[i_1]] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] <- beta_xb
    beta_xb ~ dnorm(0, sd = 1000)
    beta_x[3] <- beta_xc
    beta_xc ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[1] <- 0
    beta_x_x3[2] <- beta_xb_x3
    beta_xb_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[3] <- beta_xc_x3
    beta_xc_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Drop some terms in interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x3-1-x)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_x3 * x3[i_1] + beta_x_x3[x[i_1]] * x3[i_1]
    }
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[1] ~ dnorm(0, sd = 1000)
    beta_x_x3[2] ~ dnorm(0, sd = 1000)
    beta_x_x3[3] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_x3 = 0, beta_x_x3 = c(0, 0, 0))
  )

  # continuous-factor interaction with modelMatrixNames = TRUE
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x3, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x3 * 
            x3[i_1] + beta_x_x3[x[i_1]] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] <- beta_xb
    beta_xb ~ dnorm(0, sd = 1000)
    beta_x[3] <- beta_xc
    beta_xc ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[1] <- 0
    beta_x_x3[2] <- beta_xb_x3
    beta_xb_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[3] <- beta_xc_x3
    beta_xc_x3 ~ dnorm(0, sd = 1000)
    })
  )


  # Factor-factor interaction
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x2)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  # For reference
  mm <- model.matrix(~x*x2, as.data.frame(modInfo$constants[c("x", "x2")]))

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x2[x2[i_1]] + 
            beta_x_x2[x[i_1], x2[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] ~ dnorm(0, sd = 1000)
    beta_x_x2[1, 1] <- 0
    beta_x_x2[2, 1] <- 0
    beta_x_x2[3, 1] <- 0
    beta_x_x2[1, 2] <- 0
    beta_x_x2[2, 2] ~ dnorm(0, sd = 1000)
    beta_x_x2[3, 2] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = structure(c(0, 0, 0), dim = 3L), 
      beta_x2 = structure(c(0, 0), dim = 2L), beta_x_x2 = structure(c(0, 
      0, 0, 0, 0, 0), dim = 3:2))
  )

  # Factor-factor interaction with modelMatrixNames = TRUE
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x*x2, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  # For reference
  mm <- model.matrix(~x*x2, as.data.frame(modInfo$constants[c("x", "x2")]))

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1]] + beta_x2[x2[i_1]] + 
            beta_x_x2[x[i_1], x2[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] <- beta_xb
    beta_xb ~ dnorm(0, sd = 1000)
    beta_x[3] <- beta_xc
    beta_xc ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] <- beta_x2e
    beta_x2e ~ dnorm(0, sd = 1000)
    beta_x_x2[1, 1] <- 0
    beta_x_x2[2, 1] <- 0
    beta_x_x2[3, 1] <- 0
    beta_x_x2[1, 2] <- 0
    beta_x_x2[2, 2] <- beta_xb_x2e
    beta_xb_x2e ~ dnorm(0, sd = 1000)
    beta_x_x2[3, 2] <- beta_xc_x2e
    beta_xc_x2e ~ dnorm(0, sd = 1000)
    })
  )

  # Covariate not in constants works (but is assumed to be continuous)
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x5, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  mod$getCode()

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("LINPRED error traps LHS in formula", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), x=round(rnorm(10), 3), n = 10))

  # Missing LHS error trapping must be handled by buildMacro() in nimble
  # Enable the test below when the use3pieces error trap PR in nimble is merged
  #code <- quote(LINPRED(~1))
  
  #expect_error(
  #  LINPRED$process(code, modelInfo=modInfo, environment()),
  #  "This macro must be used as part of an assignment"
  #)

  # LHS in formula
  code <- quote(mu[1:n] <- LINPRED(y~1))
  
  expect_error(
    LINPRED$process(code, modelInfo=modInfo, environment()),
    "Formula should be RHS-only"
  )

})

test_that("LINPRED with uncorrelated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))

  # Random intercept
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (1|group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1)
  )

  # Set SD prefix
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (1|group), sdPrefix=alpha_)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    alpha_sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = alpha_sd_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         alpha_sd_group = 1)
  )

  # Set custom priors
  pr <- setPriors(intercept="dnorm(0, sd=1)", sd="dunif(0, 1)")
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (1|group), priorSpecs=pr)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 1)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    })
  )

  # Uncorrelated random continuous slopes and intercepts
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_group[group[i_1]] * x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(0, sd = sd_x_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  # Just random slopes, not intercepts
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x-1||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_x_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_x_group[i_2] ~ dnorm(0, sd = sd_x_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, 
         beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  # No fixed effects
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~0 + (x||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(0, sd = sd_x_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  # Factor random slope
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x2 + (x2||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2[x2[i_1]] + beta_group[group[i_1]] + 
            beta_x2_group[x2[i_1], group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x2d_group ~ dunif(0, 100)
    sd_x2e_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x2_group[1, i_3] ~ dnorm(0, sd = sd_x2d_group)
    }
    for (i_4 in 1:3) {
        beta_x2_group[2, i_4] ~ dnorm(0, sd = sd_x2e_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x2 = structure(c(0, 0), dim = 2L), 
      beta_group = structure(c(0, 0, 0), dim = 3L), sd_group = 1, 
      beta_x2_group = structure(c(0, 0, 0, 0, 0, 0), dim = 2:3), 
      sd_x2d_group = 1, sd_x2e_group = 1)
  )

  # Continuous-continous Interaction in random term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x:x3||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_x3_group[group[i_1]] * x[i_1] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_x3_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_x3_group[i_3] ~ dnorm(0, sd = sd_x_x3_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_x3_group = c(0, 0, 0), sd_x_x3_group = 1)
  )

  # Continuous-factor interaction in random term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x:x2||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_x2_group[x2[i_1], group[i_1]] * x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_x2d_group ~ dunif(0, 100)
    sd_x_x2e_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_x2_group[1, i_3] ~ dnorm(0, sd = sd_x_x2d_group)
    }
    for (i_4 in 1:3) {
        beta_x_x2_group[2, i_4] ~ dnorm(0, sd = sd_x_x2e_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_x2_group = structure(c(0, 0, 0, 0, 0, 0), dim = 2:3), 
         sd_x_x2d_group = 1, sd_x_x2e_group = 1)
  )

  # Factor-factor interaction in random term
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x2:x4||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x2_x4_group[x2[i_1], x4[i_1], group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x2d_x4f_group ~ dunif(0, 100)
    sd_x2e_x4f_group ~ dunif(0, 100)
    sd_x2d_x4g_group ~ dunif(0, 100)
    sd_x2e_x4g_group ~ dunif(0, 100)
    sd_x2d_x4h_group ~ dunif(0, 100)
    sd_x2e_x4h_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x2_x4_group[1, 1, i_3] ~ dnorm(0, sd = sd_x2d_x4f_group)
    }
    for (i_4 in 1:3) {
        beta_x2_x4_group[2, 1, i_4] ~ dnorm(0, sd = sd_x2e_x4f_group)
    }
    for (i_5 in 1:3) {
        beta_x2_x4_group[1, 2, i_5] ~ dnorm(0, sd = sd_x2d_x4g_group)
    }
    for (i_6 in 1:3) {
        beta_x2_x4_group[2, 2, i_6] ~ dnorm(0, sd = sd_x2e_x4g_group)
    }
    for (i_7 in 1:3) {
        beta_x2_x4_group[1, 3, i_7] ~ dnorm(0, sd = sd_x2d_x4h_group)
    }
    for (i_8 in 1:3) {
        beta_x2_x4_group[2, 3, i_8] ~ dnorm(0, sd = sd_x2e_x4h_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, 
         beta_x2_x4_group = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), dim = c(2L, 3L, 3L)), 
         sd_x2d_x4f_group = 1, sd_x2e_x4f_group = 1, sd_x2d_x4g_group = 1, 
    sd_x2e_x4g_group = 1, sd_x2d_x4h_group = 1, sd_x2e_x4h_group = 1)
  )

  # Drop terms in random expression
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x-1||group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_x_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_x_group[i_2] ~ dnorm(0, sd = sd_x_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  nimbleOptions(enableMacroComments=TRUE)
})

test_that("Centering with uncorrelated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))
  
  # Center random effects on group instead of 0
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x||group), centerVar=group)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(beta_Intercept, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(beta_x, sd = sd_x_group)
    }
  })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  # Centering when there are multiple grouping factors
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x||group) + (1|x4), centerVar=group)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1] + beta_x4[x4[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(beta_Intercept, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(beta_x, sd = sd_x_group)
    }
    sd_x4 ~ dunif(0, 100)
    for (i_4 in 1:3) {
        beta_x4[i_4] ~ dnorm(0, sd = sd_x4)
    }
    })
  )

  # Centering with no intercept
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~0 + x + (x||group), centerVar=group)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(beta_x, sd = sd_x_group)
    }
  })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  # Centering when there are no fixed effects
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~0 + (x||group), centerVar=group)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    sd_x_group ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x_group[i_3] ~ dnorm(0, sd = sd_x_group)
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1)
  )

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("noncentered parameterization with uncorrelated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x||group), noncentered=TRUE)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_group[group[i_1]] * x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group_raw[i_2] ~ dnorm(0, sd = 1)
    }
    for (i_3 in 1:3) {
        beta_group[i_3] <- 0 + sd_group * beta_group_raw[i_3]
    }
    sd_x_group ~ dunif(0, 100)
    for (i_4 in 1:3) {
        beta_x_group_raw[i_4] ~ dnorm(0, sd = 1)
    }
    for (i_5 in 1:3) {
        beta_x_group[i_5] <- 0 + sd_x_group * beta_x_group_raw[i_5]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
      sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1, 
      beta_group_raw = structure(c(0, 0, 0), dim = 3L), beta_x_group_raw = c(0, 0, 0))
  )

  # With centering variable also
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x||group), noncentered=TRUE, centerVar=group)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group_raw[i_2] ~ dnorm(0, sd = 1)
    }
    for (i_3 in 1:3) {
        beta_group[i_3] <- beta_Intercept + sd_group * beta_group_raw[i_3]
    }
    sd_x_group ~ dunif(0, 100)
    for (i_4 in 1:3) {
        beta_x_group_raw[i_4] ~ dnorm(0, sd = 1)
    }
    for (i_5 in 1:3) {
        beta_x_group[i_5] <- beta_x + sd_x_group * beta_x_group_raw[i_5]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
      sd_group = 1, beta_x_group = c(0, 0, 0), sd_x_group = 1, 
      beta_group_raw = structure(c(0, 0, 0), dim = 3L), beta_x_group_raw = c(0, 0, 0))
  )

  # Factor slope
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x2||group), noncentered=TRUE)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x2_group[x2[i_1], group[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_group_raw[i_2] ~ dnorm(0, sd = 1)
    }
    for (i_3 in 1:3) {
        beta_group[i_3] <- 0 + sd_group * beta_group_raw[i_3]
    }
    sd_x2d_group ~ dunif(0, 100)
    sd_x2e_group ~ dunif(0, 100)
    for (i_4 in 1:3) {
        beta_x2_group_raw[1, i_4] ~ dnorm(0, sd = 1)
    }
    for (i_5 in 1:3) {
        beta_x2_group[1, i_5] <- 0 + sd_x2d_group * beta_x2_group_raw[1, 
            i_5]
    }
    for (i_6 in 1:3) {
        beta_x2_group_raw[2, i_6] ~ dnorm(0, sd = 1)
    }
    for (i_7 in 1:3) {
        beta_x2_group[2, i_7] <- 0 + sd_x2e_group * beta_x2_group_raw[2, 
            i_7]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         sd_group = 1, beta_x2_group = structure(c(0, 0, 0, 0, 0, 0), dim = 2:3), 
         sd_x2d_group = 1, sd_x2e_group = 1, beta_group_raw = structure(c(0, 0, 0), dim = 3L), 
         beta_x2_group_raw = structure(c(0, 0, 0, 0, 0, 0), dim = 2:3))
  )

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("correlated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x|group))
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_group[group[i_1]] * x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    sd_x_group ~ dunif(0, 100)
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1, 2)
    U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 
        1:2], re_sds_group[1:2])
    re_means_group[1] <- 0
    re_means_group[2] <- 0
    for (i_2 in 1:3) {
        B_group[i_2, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 
            1:2], prec_param = 0)
        beta_group[i_2] <- B_group[i_2, 1]
        beta_x_group[i_2] <- B_group[i_2, 2]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         beta_x_group = c(0, 0, 0), sd_group = 1, sd_x_group = 1)
  )

  # Set LKJ shape value
  pr <- setPriors(lkjShape=3)
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x|group), priorSpecs=pr)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_group[group[i_1]] * x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    sd_x_group ~ dunif(0, 100)
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(3, 2)
    U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 
        1:2], re_sds_group[1:2])
    re_means_group[1] <- 0
    re_means_group[2] <- 0
    for (i_2 in 1:3) {
        B_group[i_2, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 
            1:2], prec_param = 0)
        beta_group[i_2] <- B_group[i_2, 1]
        beta_x_group[i_2] <- B_group[i_2, 2]
    }
    })
  )

  # More than two correlated params
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x + x3|group))
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]] + 
            beta_x_group[group[i_1]] * x[i_1] + beta_x3_group[group[i_1]] * 
            x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    sd_x_group ~ dunif(0, 100)
    sd_x3_group ~ dunif(0, 100)
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    re_sds_group[3] <- sd_x3_group
    Ustar_group[1:3, 1:3] ~ dlkj_corr_cholesky(1, 3)
    U_group[1:3, 1:3] <- uppertri_mult_diag(Ustar_group[1:3, 
        1:3], re_sds_group[1:3])
    re_means_group[1] <- 0
    re_means_group[2] <- 0
    re_means_group[3] <- 0
    for (i_2 in 1:3) {
        B_group[i_2, 1:3] ~ dmnorm(re_means_group[1:3], cholesky = U_group[1:3, 
            1:3], prec_param = 0)
        beta_group[i_2] <- B_group[i_2, 1]
        beta_x_group[i_2] <- B_group[i_2, 2]
        beta_x3_group[i_2] <- B_group[i_2, 3]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         beta_x_group = c(0, 0, 0), beta_x3_group = c(0, 0, 0), sd_group = 1, 
         sd_x_group = 1, sd_x3_group = 1)
  )

  # Factor random slopes don't work
  code <- quote(LINPRED_PRIORS(~x + (x2|group)))
  expect_error(LINPRED_PRIORS$process(code, modInfo, NULL), "Correlated")

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("Centering with correlated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (x|group), centerVar=group)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    sd_x_group ~ dunif(0, 100)
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1, 2)
    U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 
        1:2], re_sds_group[1:2])
    re_means_group[1] <- beta_Intercept
    re_means_group[2] <- beta_x
    for (i_2 in 1:3) {
        B_group[i_2, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 
            1:2], prec_param = 0)
        beta_group[i_2] <- B_group[i_2, 1]
        beta_x_group[i_2] <- B_group[i_2, 2]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         beta_x_group = c(0, 0, 0), sd_group = 1, sd_x_group = 1)
  )

  # With intercept dropped
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~0 + x + (x|group), centerVar=group)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_group[group[i_1]] + beta_x_group[group[i_1]] * 
            x[i_1]
    }
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    sd_x_group ~ dunif(0, 100)
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1, 2)
    U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 
        1:2], re_sds_group[1:2])
    re_means_group[1] <- 0
    re_means_group[2] <- beta_x
    for (i_2 in 1:3) {
        B_group[i_2, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 
            1:2], prec_param = 0)
        beta_group[i_2] <- B_group[i_2, 1]
        beta_x_group[i_2] <- B_group[i_2, 2]
    }
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_x = 0, beta_group = structure(c(0, 0, 0), dim = 3L), 
         beta_x_group = c(0, 0, 0), sd_group = 1, sd_x_group = 1)
  )

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("Noncentered parameterization doesn't work with correlated random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))
  # Noncentered doesn't work with correlated random effects  
  code <- quote(LINPRED_PRIORS(~x + (x|group), noncentered=TRUE))
  expect_error(LINPRED_PRIORS$process(code, modInfo, NULL)$code, "Noncentered")

  nimbleOptions(enableMacroComments = TRUE)
})


test_that("LINPRED with brackets on RHS of formula", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), 
                  x=matrix(sample(letters[1:3], 10, replace=T), ncol=1),
                  x2=factor(sample(letters[4:5], 10, replace=T)),
                  site = sample(1:10, 10, replace=TRUE),
                  x3=round(rnorm(10),3), x4=round(rnorm(10), 3), n = 10, J =3),
                  inits=list(beta_Intercept=0))

  # Different dimensions on RHS
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x[1:n, 1] + x3)
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1, 1]] + beta_x3 * 
            x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Nested indexing
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x2 + x3[site[1:n]])
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x2[x2[i_1]] + beta_x3 * 
            x3[site[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    })
  )

  # Interaction with different indices
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x[1:n,1]*x3[site[1:n]])
  })
  mod <- nimbleModel(code, constants = modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1, 1]] + beta_x3 * 
            x3[site[i_1]] + beta_x_x3[x[i_1, 1]] * x3[site[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    beta_x_x3[1] <- 0
    beta_x_x3[2] ~ dnorm(0, sd = 1000)
    beta_x_x3[3] ~ dnorm(0, sd = 1000)
    })
  )

  # Random effect
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~(x3[site[1:n]]||x[1:n,1]))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x[x[i_1, 1]] + beta_x3_x[x[i_1, 
            1]] * x3[site[i_1]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    sd_x ~ dunif(0, 100)
    for (i_2 in 1:3) {
        beta_x[i_2] ~ dnorm(0, sd = sd_x)
    }
    sd_x3_x ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_x3_x[i_3] ~ dnorm(0, sd = sd_x3_x)
    }
    })
  )

  nimbleOptions(enableMacroComments = TRUE)
})


test_that("LINPRED with factor array covariate", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants=list(y=matrix(rnorm(12), 3, 4),
                                 x=matrix(sample(letters[1:3], 12, replace=T), 3, 4),
                                 M=3, J=4))

  code <- nimbleCode({
    mu[1:M, 1:J] <- LINPRED(~x)
  })
  mod <- nimbleModel(code, constants=modInfo$constants)
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:M) {
        for (i_2 in 1:J) {
            mu[i_1, i_2] <- beta_Intercept + beta_x[x[i_1, i_2]]
        }
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x[1] <- 0
    beta_x[2] ~ dnorm(0, sd = 1000)
    beta_x[3] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(dim(mod$getConstants()$x), dim(modInfo$constants$x))
  nimbleOptions(enableMacroComments = TRUE)
})


test_that("Nested random effects", {
  nimbleOptions(enableMacroComments = FALSE)
  set.seed(123)
  modInfo <- list(constants=list(y = rnorm(10), 
                               x=factor(sample(letters[1:3], 10, replace=T)),
                               w = factor(sample(letters[6:8], 10, replace=T)),
                    x3=round(rnorm(10),3), n=10),
                indexCreator=nimble:::labelFunctionCreator("i"))

  # w:x notation
  # Linear predictor
  code <- quote(mu[1:n] <- LINPRED(~x3 + (x3||x:w), priorSpecs=NULL))
  out <- LINPRED$process(code, modelInfo=modInfo, NULL)
  # Make sure new combined levels constant is added
  expect_equal(
    out$modelInfo$constants$x_w,
    factor(paste(modInfo$constants$x, modInfo$constants$w, sep=":"))
  )
  expect_equal(length(levels(out$modelInfo$constants$x_w)), 7)

  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3 + (x3||x:w))
  })
  mod <- nimbleModel(code, constants=modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x3 * x3[i_1] + beta_x_w[x_w[i_1]] + 
            beta_x3_x_w[x_w[i_1]] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    sd_x_w ~ dunif(0, 100)
    for (i_2 in 1:7) {
        beta_x_w[i_2] ~ dnorm(0, sd = sd_x_w)
    }
    sd_x3_x_w ~ dunif(0, 100)
    for (i_3 in 1:7) {
        beta_x3_x_w[i_3] ~ dnorm(0, sd = sd_x3_x_w)
    }
  })
  )
  expect_equal(mod$getConstants()$x_w, as.numeric(out$modelInfo$constants$x_w))

  # w/x notation, which adds |w random effect(s)
  bars <- reformulas::findbars(~x3 + (x3||w/x)) # for reference
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x3 + (x3||w/x))
  })
  mod <- nimbleModel(code, constants=modInfo$constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x3 * x3[i_1] + beta_x_w[x_w[i_1]] + 
            beta_w[w[i_1]] + beta_x3_x_w[x_w[i_1]] * x3[i_1] + 
            beta_x3_w[w[i_1]] * x3[i_1]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x3 ~ dnorm(0, sd = 1000)
    sd_x_w ~ dunif(0, 100)
    for (i_2 in 1:7) {
        beta_x_w[i_2] ~ dnorm(0, sd = sd_x_w)
    }
    sd_w ~ dunif(0, 100)
    for (i_3 in 1:3) {
        beta_w[i_3] ~ dnorm(0, sd = sd_w)
    }
    sd_x3_x_w ~ dunif(0, 100)
    for (i_4 in 1:7) {
        beta_x3_x_w[i_4] ~ dnorm(0, sd = sd_x3_x_w)
    }
    sd_x3_w ~ dunif(0, 100)
    for (i_5 in 1:3) {
        beta_x3_w[i_5] ~ dnorm(0, sd = sd_x3_w)
    }
    }) 
  )
  expect_equal(mod$getConstants()$x_w, as.numeric(out$modelInfo$constants$x_w))

  nimbleOptions(enableMacroComments = TRUE)
})


test_that("Macro comments work with LINPRED", {

  nimbleOptions(enableMacroComments = TRUE)
  set.seed(123)
  modInfo <- list(constants= list(y = rnorm(10), group=factor(sample(letters[1:3], 10, replace=T)),
                    x2=factor(sample(letters[4:5], 10, replace=T)),
                    x=round(rnorm(10),3), x3=round(rnorm(10), 3), 
                    x4 = factor(sample(letters[6:8], 10, replace=T)), n=10))

  # Random intercept
  code <- nimbleCode({
    mu[1:n] <- LINPRED(~x + (1|group))
  })
  mod <- nimbleModel(code, constants = modInfo$constants)
  
  expect_equal(
    mod$getCode(),
    quote({
    "# LINPRED"
    "  ## nimbleMacros::FORLOOP"
    for (i_1 in 1:n) {
        mu[i_1] <- beta_Intercept + beta_x * x[i_1] + beta_group[group[i_1]]
    }
    "  ## ----"
    "  ## nimbleMacros::LINPRED_PRIORS"
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    sd_group ~ dunif(0, 100)
    "    ### nimbleMacros::FORLOOP"
    for (i_2 in 1:3) {
        beta_group[i_2] ~ dnorm(0, sd = sd_group)
    }
    "    ### ----"
    "  ## ----"
    "# ----"
    })
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

test_that("processNestedRandomEffects", {
  dat <- list(group=factor(c("a","b","c")), group2=factor(c("d","e","f")))
  
  out1 <- processNestedRandomEffects(quote(1|group), dat)
  expect_equal(out1$barExp, quote(1|group))
  expect_equal(out1$constants, NULL)

  out2 <- processNestedRandomEffects(quote(1|group:group2), dat)
  expect_equal(out2$barExp, quote(1|group_group2))
  expect_equal(out2$constants, list(group_group2=factor(c("a:d", "b:e", "c:f"))))
  
  # Check the new factor is not duplicated
  dat2 <- c(dat, out2$constants)
  out3 <- processNestedRandomEffects(quote(1|group:group2), dat2)
  expect_equal(out3$constants, list())

  # Handle character matrices
  dat2 <- list(group=matrix(c("a","b","b","a"), 2, 2),
              group2=matrix(c("c","d","c","d"), 2, 2))

  out4 <- processNestedRandomEffects(quote(1|group:group2), dat2)
  expect_equal(out4$constants$group_group2,
               matrix(c("a:c","b:d","b:c","a:d"), 2, 2))
  
  # Mismatched array sizes should error
  dat3 <- list(group=matrix(c("a","b","b","a","z","z"), 3, 2),
              group2=matrix(c("c","d","c","d"), 2, 2))
  expect_error(processNestedRandomEffects(quote(1|group:group2), dat3))
})
