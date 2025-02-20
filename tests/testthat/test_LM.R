context("LM and related functions")

skip_on_cran()

test_that("LM for linear regression", {
  nimbleOptions(enableMacroComments = FALSE)
  dat <- list(x = rnorm(3), x2 = factor(c("a","b","c")), y = rnorm(3))
  modelInfo <- list(constants=dat)
  
  code <- nimbleCode({
    LM(y ~ x + x2, priorSpecs=setPriors(sd="dunif(0, 5)"))
  })
  mod <- nimbleModel(code, constants=dat)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:3) {
        mu_[i_2] <- beta_Intercept + beta_x * x[i_2] + beta_x2[x2[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] ~ dnorm(0, sd = 1000)
    beta_x2[3] ~ dnorm(0, sd = 1000)
    sd_residual ~ dunif(0, 5)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(sd_residual = 1, beta_Intercept = 0, beta_x = 0, beta_x2 = structure(c(0, 0, 0), dim = 3L))
  )
  expect_equal(mod$getConstants()$x2, c(1,2,3))

  # With model matrix names
  code <- quote(LM(y ~ x + x2, priorSpecs=setPriors(sd="dunif(0, 5)"), modelMatrixNames=TRUE))
  out <- LM$process(code, modelInfo, environment())

  code <- nimbleCode({
    LM(y ~ x + x2, modelMatrixNames = TRUE)
  })
  mod <- nimbleModel(code, constants=dat)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:3) {
        mu_[i_2] <- beta_Intercept + beta_x * x[i_2] + beta_x2[x2[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] <- beta_x2b
    beta_x2b ~ dnorm(0, sd = 1000)
    beta_x2[3] <- beta_x2c
    beta_x2c ~ dnorm(0, sd = 1000)
    sd_residual ~ dunif(0, 100)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(sd_residual = 1, beta_Intercept = 0, beta_x = 0, beta_x2 = structure(c(0, 0, 0), dim = 3L))
  )

  # With custom prefix
  code <- nimbleCode({
    LM(y ~ x + x2, coefPrefix=alpha_)
  })
  mod <- nimbleModel(code, constants=dat)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:3) {
        mu_[i_2] <- alpha_Intercept + alpha_x * x[i_2] + alpha_x2[x2[i_2]]
    }
    alpha_Intercept ~ dnorm(0, sd = 1000)
    alpha_x ~ dnorm(0, sd = 1000)
    alpha_x2[1] <- 0
    alpha_x2[2] ~ dnorm(0, sd = 1000)
    alpha_x2[3] ~ dnorm(0, sd = 1000)
    sd_residual ~ dunif(0, 100)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(sd_residual=1, alpha_Intercept = 0, alpha_x = 0, alpha_x2 = structure(c(0, 0, 0), dim = 3L))
  )

  # Doesn't overwrite provided inits
  code <- nimbleCode({
    LM(y ~ x + x2)
  })
  mod <- nimbleModel(code, constants=dat, inits=list(beta_Intercept=2))
  expect_equal(mod$beta_Intercept, 2)

  nimbleOptions(enableMacroComments = TRUE)
})
 
test_that("LM with Poisson regression", {
  # Poisson example
  nimbleOptions(enableMacroComments = FALSE)
  dat <- list(x = rnorm(3), x2 = factor(c("a","b","c")), y = rpois(3, lambda=1))
  modelInfo <- list(constants=dat)
  
  code <- nimbleCode({
    LM(y ~ x + x2, family=poisson(link=log))
  })
  mod <- nimbleModel(code, constants=dat)
  
  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dpois(mu_[i_1])
    }
    for (i_2 in 1:3) {
        log(mu_[i_2]) <- beta_Intercept + beta_x * x[i_2] + beta_x2[x2[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    beta_x2[1] <- 0
    beta_x2[2] ~ dnorm(0, sd = 1000)
    beta_x2[3] ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0, beta_x2 = structure(c(0, 0, 0), dim = 3L))
  )

  nimbleOptions(enableMacroComments = TRUE)
})
 

test_that("LM with binomial regression", {

  nimbleOptions(enableMacroComments = FALSE)
  
  # Binomial example (aggregated)
  constants <- list(y=c(1,2,3), ny = c(0,1,3), x=rnorm(3))
  modelInfo <- list(constants=constants)

  code <- nimbleCode({
    LM(cbind(y, ny) ~ x, family=binomial)
  })
  mod <- nimbleModel(code, constants=constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dbinom(mu_[i_1], size = binSize[i_1])
    }
    for (i_2 in 1:3) {
        logit(mu_[i_2]) <- beta_Intercept + beta_x * x[i_2]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )
  expect_equal(
    mod$modelDef$macroInits,
    list(beta_Intercept = 0, beta_x = 0)
  )
  # Make sure sample size was calculated and added
  expect_equal(mod$getConstants()$binSize,
               constants$y + constants$ny)

  # Binomial example (binary)
  constants <- list(y=c(0,1,0), x=rnorm(3))
  code <- nimbleCode({
    LM(y ~ x, family=binomial)
  })
  mod <- nimbleModel(code, constants=constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dbinom(mu_[i_1], size = 1)
    }
    for (i_2 in 1:3) {
        logit(mu_[i_2]) <- beta_Intercept + beta_x * x[i_2]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )
  expect_true(is.null(mod$getConstants()$binSize))

  # Different link function
  code <- nimbleCode({
    LM(y ~ x, family=binomial(link=probit))
  })
  mod <- nimbleModel(code, constants=constants)

  expect_equal(
    mod$getCode(),
    quote({
    for (i_1 in 1:3) {
        y[i_1] ~ dbinom(mu_[i_1], size = 1)
    }
    for (i_2 in 1:3) {
        probit(mu_[i_2]) <- beta_Intercept + beta_x * x[i_2]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_x ~ dnorm(0, sd = 1000)
    })
  )

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("Run lm example", { 
  skip_on_ci()
  skip_on_cran()
  nimbleOptions(enableMacroComments = FALSE)
  ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
  ## Page 9: Plant Weight Data.
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- stats::lm(weight ~ group)

  code.D9 <- nimbleCode({
    LM(weight ~ group, modelMatrixNames = TRUE)
  })
  constants=data.frame(weight=weight, group=group)
  mod.D9 <- nimbleModel(code.D9, constants=constants)
  expect_equal(
    mod.D9$getCode(),
    quote({
    for (i_1 in 1:20) {
        weight[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:20) {
        mu_[i_2] <- beta_Intercept + beta_group[group[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_group[1] <- 0
    beta_group[2] <- beta_groupTrt
    beta_groupTrt ~ dnorm(0, sd = 1000)
    sd_residual ~ dunif(0, 100)
    })
  )
  set.seed(123)
  nim.D9 <- nimbleMCMC(mod.D9, nchains=1, niter=2000, nburnin=1000,
                       progressBar=FALSE)
  nim_est <- apply(nim.D9, 2, median)
  lm_est <- c(coef(lm.D9), sigma=sigma(lm.D9))
  comp <- cbind(lm=lm_est, nimble=nim_est) # comparison
  expect_equivalent(round(nim_est, 2), c(5.06, -0.40, 0.74))

  lm.D90 <- stats::lm(weight ~ group - 1) # omitting intercept
  code.D90 <- nimbleCode({
    LM(weight ~ group - 1, modelMatrixNames = TRUE)
  })
  mod.D90 <- nimbleModel(code.D90, constants=constants)
  expect_equal(
    mod.D90$getCode(),
    quote({
    for (i_1 in 1:20) {
        weight[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:20) {
        mu_[i_2] <- beta_group[group[i_2]]
    }
    beta_group[1] <- beta_groupCtl
    beta_groupCtl ~ dnorm(0, sd = 1000)
    beta_group[2] <- beta_groupTrt
    beta_groupTrt ~ dnorm(0, sd = 1000)
    sd_residual ~ dunif(0, 100)
    })
  )
  set.seed(123)
  nim.D90 <- nimbleMCMC(mod.D90, nchains=1, niter=2000, nburnin=1000,
                       progressBar=FALSE)
  nim_est <- apply(nim.D90, 2, median)
  lm_est <- c(coef(lm.D90), sigma=sigma(lm.D90))
  comp <- cbind(lm=lm_est, nimble=nim_est) # comparison
  expect_equivalent(round(nim_est, 2), c(5.03, 4.65, 0.74))

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("Run glm example", {
  skip_on_ci()
  skip_on_cran()
  nimbleOptions(enableMacroComments = FALSE)

  ## Dobson (1990) Page 93: Randomized Controlled Trial :
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  glm.D93 <- stats::glm(counts ~ outcome + treatment, family = poisson())
  
  constants <- list(treatment=treatment, outcome=outcome, counts=counts)
  pr <- setPriors(intercept="dnorm(0, sd=10)", coefficient="dnorm(0, sd = 10)")
  code.D93 <- nimbleCode({
    LM(counts ~ outcome + treatment, family = poisson(), 
       modelMatrixNames=TRUE, priorSpecs=pr)
  })
  mod.D93 <- nimbleModel(code.D93, constants=constants)

  expect_equal(
    mod.D93$getCode(),
    quote({
    for (i_1 in 1:9) {
        counts[i_1] ~ dpois(mu_[i_1])
    }
    for (i_2 in 1:9) {
        log(mu_[i_2]) <- beta_Intercept + beta_outcome[outcome[i_2]] + 
            beta_treatment[treatment[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 10)
    beta_outcome[1] <- 0
    beta_outcome[2] <- beta_outcome2
    beta_outcome2 ~ dnorm(0, sd = 10)
    beta_outcome[3] <- beta_outcome3
    beta_outcome3 ~ dnorm(0, sd = 10)
    beta_treatment[1] <- 0
    beta_treatment[2] <- beta_treatment2
    beta_treatment2 ~ dnorm(0, sd = 10)
    beta_treatment[3] <- beta_treatment3
    beta_treatment3 ~ dnorm(0, sd = 10)
    })
  )

  set.seed(123)
  nim.D93 <- nimbleMCMC(mod.D93, nchains=1, niter=5000, nburnin=4000,
                       progressBar=FALSE)
  nim_est <- round(apply(nim.D93, 2, median), 3)
  glm_est <- round(coef(glm.D93), 3)
  comp <- cbind(glm=glm_est, nimble=nim_est) # comparison
  expect_equivalent(nim_est, c(3.054, -0.451, -0.319, -0.009, -0.023))

  nimbleOptions(enableMacroComments = FALSE)
})

test_that("Run lme4::lmer() example", {
  skip_on_cran()
  nimbleOptions(enableMacroComments = FALSE)
  sleepstudy <- readRDS('sleepstudy.Rds') # from lme4 package
  sleepstudy <- subset(sleepstudy, Days>=2)

  #library(lme4)
  #fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)

  code_fm1 <- nimbleCode({
    LM(Reaction ~ Days + (Days|Subject))
  })
  mod_fm1 <- nimbleModel(code_fm1, constants=as.list(sleepstudy))

  expect_equal(
    mod_fm1$getCode(),
    quote({
    for (i_1 in 1:144) {
        Reaction[i_1] ~ dnorm(mu_[i_1], sd = sd_residual)
    }
    for (i_2 in 1:144) {
        mu_[i_2] <- beta_Intercept + beta_Days * Days[i_2] + 
            beta_Subject[Subject[i_2]] + beta_Days_Subject[Subject[i_2]] * Days[i_2]
    }
    beta_Intercept ~ dnorm(0, sd = 1000)
    beta_Days ~ dnorm(0, sd = 1000)
    sd_Subject ~ dunif(0, 100)
    sd_Days_Subject ~ dunif(0, 100)
    re_sds_Subject[1] <- sd_Subject
    re_sds_Subject[2] <- sd_Days_Subject
    Ustar_Subject[1:2, 1:2] ~ dlkj_corr_cholesky(1, 2)
    U_Subject[1:2, 1:2] <- uppertri_mult_diag(Ustar_Subject[1:2, 
        1:2], re_sds_Subject[1:2])
    re_means_Subject[1] <- 0
    re_means_Subject[2] <- 0
    for (i_3 in 1:18) {
        B_Subject[i_3, 1:2] ~ dmnorm(re_means_Subject[1:2], cholesky = U_Subject[1:2, 
            1:2], prec_param = 0)
        beta_Subject[i_3] <- B_Subject[i_3, 1]
        beta_Days_Subject[i_3] <- B_Subject[i_3, 2]
    }
    sd_residual ~ dunif(0, 100)
    })
  )

  set.seed(123)
  nim_fm1 <- nimbleMCMC(mod_fm1, nchains=1, niter=30000, nburnin=25000,
                        progressBar=FALSE)
  nim_est <- round(apply(nim_fm1, 2, median), 3)
  nim_est <- nim_est[c(6,5,8,7,3,9)]

  
  #vc <- attributes(VarCorr(fm1)$Subject)
  #lmer_est <- round(c(fixef(fm1), vc$stddev, vc$correlation[1,2], sigma(fm1)), 3)
  #names(lmer_est) <- c("Intercept", "Days", "1|Subject SD",
  #                     "Days|Subject SD", "Cor", "sigma")
  lmer_est <- c(Intercept = 245.097, Days = 11.435, `1|Subject SD` = 31.507, 
              `Days|Subject SD` = 6.766, Cor = -0.255, sigma = 25.526)
  
  comp <- cbind(lmer=lmer_est, nimble=nim_est) # comparison
  expect_equivalent(nim_est, c(247.552, 11.408, 33.200, 7.267, -0.214, 25.807))

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("Run lme4::glmer() example", {
  skip_on_ci()
  skip_on_cran()
  nimbleOptions(enableMacroComments = FALSE)
  cbpp <- readRDS("cbpp.Rds") # from lme4 package
  
  #library(lme4)
  #gm1 <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
  #              data = cbpp, family = binomial)

  constants <- as.list(cbpp)
  constants$noncase <- constants$size - constants$incidence
  pr <- setPriors(intercept="dnorm(0, sd=10)", coefficient="dnorm(0, sd=10)")
  code_gm1 <- nimbleCode({
    LM(cbind(incidence, noncase) ~ period + (1 | herd), priorSpecs=pr, 
       family=binomial, modelMatrixNames=TRUE)
  })
  mod_gm1 <- nimbleModel(code_gm1, constants=constants)
  
  expect_equal(
    mod_gm1$getCode(),
    quote({
    for (i_1 in 1:56) {
        incidence[i_1] ~ dbinom(mu_[i_1], size = binSize[i_1])
    }
    for (i_2 in 1:56) {
        logit(mu_[i_2]) <- beta_Intercept + beta_period[period[i_2]] + 
            beta_herd[herd[i_2]]
    }
    beta_Intercept ~ dnorm(0, sd = 10)
    beta_period[1] <- 0
    beta_period[2] <- beta_period2
    beta_period2 ~ dnorm(0, sd = 10)
    beta_period[3] <- beta_period3
    beta_period3 ~ dnorm(0, sd = 10)
    beta_period[4] <- beta_period4
    beta_period4 ~ dnorm(0, sd = 10)
    sd_herd ~ dunif(0, 100)
    for (i_3 in 1:15) {
        beta_herd[i_3] ~ dnorm(0, sd = sd_herd)
    }
    })
  )

  set.seed(123)
  nim_gm1 <- nimbleMCMC(mod_gm1, nchains=1, niter=30000, nburnin=25000,
                        progressBar=FALSE)
  nim_est <- round(apply(nim_gm1, 2, median), 3)

  #vc <- attributes(VarCorr(gm1)$herd)
  #glmer_est <- round(c(fixef(gm1), vc$stddev), 3)
  #names(glmer_est)[5] <- "herd SD"
  glmer_est <- c(`(Intercept)` = -1.398, period2 = -0.992, period3 = -1.128, 
                period4 = -1.58, `herd SD` = 0.642)

  comp <- cbind(glmer=glmer_est, nimble=nim_est) # comparison
  expect_equivalent(nim_est, c(-1.431,-0.979,-1.135,-1.616,0.752))

  nimbleOptions(enableMacroComments = TRUE)
})

test_that("processFamily", {
  expect_equal(
    processFamily("poisson")$link,
    "log"
  )
  expect_equal(
    processFamily(quote(poisson))$link,
    "log"
  )
  expect_equal(
    processFamily(poisson(link='log'))$link,
    "log"
  )
  expect_equal(
    processFamily(quote(gaussian))$link,
    "identity"
  )
})

test_that("getDataDistCode", {
 
  expect_equal(
    getDataDistCode("gaussian", quote(y), quote(1:n), quote(sd)),
    quote(y ~ FORLOOP(dnorm(mu_[1:n], sd = sd)))
  )

  expect_equal(
    getDataDistCode("poisson", quote(y), quote(1:n), quote(sd)),
    quote(y ~ FORLOOP(dpois(mu_[1:n])))
  )

  expect_error(
    getDataDistCode("Gamma", quote(y), quote(1:n))
  )

})

test_that("Missing data error trap", {
  modelInfo <- list(constants=  list(x = rnorm(3), x2 = factor(c("a","b","c"))))

  code <- quote(LM(y ~ x + x2))
  expect_error(LM$process(code, modelInfo, environment()), "dimensions for")
})
