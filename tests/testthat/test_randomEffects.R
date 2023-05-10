context("Random effects functions")

test_that("isBar", {
  expect_true(isBar(quote(1|group)))
  expect_false(isBar(quote(group)))
})

test_that("barToTerms", {  
  expect_equal(
    barToTerms(quote(1|group)),
    quote(group)
  )
  expect_equal(
    barToTerms(quote(x|group)),
    list(quote(group), x=quote(x:group))
  )
  expect_equal(
    barToTerms(quote(x-1|group)),
    list(x=quote(x:group))
  )
  expect_equal(
    barToTerms(quote(x+0|group)),
    list(x=quote(x:group))
  )
  expect_equal(
    barToTerms(quote(x*y|group)),
    list(quote(group), x=quote(x:group), y=quote(y:group), `x:y`=quote(x:y:group))
  )
})

test_that("getRandomFactorName", {
  expect_equal(
    getRandomFactorName(quote(1|group)),
    quote(group)
  )
  expect_equal(
    getRandomFactorName(quote(x|group2)),
    quote(group2)
  )
})

test_that("getCombinedFormulaFromBar", {
  expect_equal(
    getCombinedFormulaFromBar(quote(1|group)),
    quote(group)
  )
  expect_equal(
    getCombinedFormulaFromBar(quote(x|group)),
    quote(group + x:group)
  )
  expect_equal(
    getCombinedFormulaFromBar(quote(x-1|group)),
    quote(x:group)
  )
  expect_equal(
    getCombinedFormulaFromBar(quote(x*y|group)),
    quote(group + x:group + y:group + x:y:group)
  )
})

test_that("addFormulaTerms", {
  expect_equal(
    addFormulaTerms(list(quote(group))),
    quote(group)
  )
  expect_equal(
    addFormulaTerms(list(quote(group), quote(x:group))),
    quote(group + x:group)
  )
})

test_that("getHyperpriorNames", {
  expect_equal(
    getHyperpriorNames(quote(1|group), quote(beta_)),
    list(beta_sd_group=quote(beta_sd_group))
  )
  expect_equal(
    getHyperpriorNames(quote(x|group), NULL),
    list(sd_group=quote(sd_group), sd_x_group=quote(sd_x_group))
  )
  expect_equal(
    getHyperpriorNames(quote(x-1|group), NULL),
    list(sd_x_group=quote(sd_x_group))
  )
  expect_equal(
    getHyperpriorNames(quote(x*y|group), NULL),
    list(sd_group=quote(sd_group), sd_x_group=quote(sd_x_group),
         sd_y_group=quote(sd_y_group), sd_x_y_group=quote(sd_x_y_group))
  )
})

test_that("makeHyperpriorCode", {  
  expect_equal(
    makeHyperpriorCode(quote(1|group), quote(alpha_), quote(dunif(0, 3))),
    quote({
      alpha_sd_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x|group), NULL, quote(dunif(0, 3))),
    quote({
      sd_group ~ dunif(0, 3)
      sd_x_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x-1|group), NULL, quote(dunif(0, 3))),
    quote({
      sd_x_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x*y|group), NULL, quote(dunif(0, 3))),
    quote({
      sd_group ~ dunif(0, 3)
      sd_x_group ~ dunif(0, 3)
      sd_y_group ~ dunif(0, 3)
      sd_x_y_group ~ dunif(0, 3)
    })
  )
})

test_that("makeRandomParNames", {  
  expect_equal(
    makeRandomParNames(quote(1|group), quote(beta_)),
    list(beta_group=quote(beta_group))
  )
  expect_equal(
    makeRandomParNames(quote(x|group), quote(beta_)),
    list(beta_group=quote(beta_group), beta_x_group=quote(beta_x_group))
  )
  expect_equal(
    makeRandomParNames(quote(x-1|group), quote(beta_)),
    list(beta_x_group=quote(beta_x_group))
  )
  expect_equal(
    makeRandomParNames(quote(x*y|group), quote(beta_)),
    list(beta_group=quote(beta_group), beta_x_group=quote(beta_x_group),
         beta_y_group=quote(beta_y_group), beta_x_y_group=quote(beta_x_y_group))
  )
})

test_that("numRandomFactorLevels", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  expect_equal(
    numRandomFactorLevels(quote(1|group), dat),
    3
  )
  expect_error(numRandomFactorLevels(quote(1|x), dat))
})

test_that("makeUncorrelatedRandomPrior", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  expect_equal(
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta_), NULL, dat),
    quote(beta_group[1:3] ~ forLoop(dnorm(0, sd = sd_group)))
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta_), quote(alpha_), dat),
    quote(beta_group[1:3] ~ forLoop(dnorm(0, sd = alpha_sd_group)))
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(x-1|group), quote(beta_), NULL, dat),
    quote(beta_x_group[1:3] ~ forLoop(dnorm(0, sd = sd_x_group)))
  )
  # Not an uncorrelated random effect
  expect_error(
    makeUncorrelatedRandomPrior(quote(x|group), quote(beta_), NULL, dat)
  )
})

test_that("makeCorrelatedRandomPrior", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  idxCreator <- nimble:::labelFunctionCreator("i")
  out <- makeCorrelatedRandomPrior(quote(x|group), quote(beta_), NULL, dat, indexCreator=idxCreator)
  expect_equal(
    out$code,
    quote({
    {
        re_sds_group[1] <- sd_group
        re_sds_group[2] <- sd_x_group
    }
    {
        Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
        U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 1:2], re_sds_group[1:2])
    }
    re_means_group[1:2] <- rep(0, 2)
    for (i_1 in 1:3) {
        B_group[i_1, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 1:2], prec_param = 0)
        beta_group[i_1] <- B_group[i_1, 1]
        beta_x_group[i_1] <- B_group[i_1, 2]
    }


    })
  )

  expect_error(
    makeCorrelatedRandomPrior(quote(1|group), quote(beta_), NULL, dat)
  )
})

test_that("makeRandomPriorCode", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  idxCreator <- nimble:::labelFunctionCreator("i")
  out1 <- makeRandomPriorCode(quote(x+0|group), quote(beta_), NULL, dat, indexCreator=idxCreator)
  expect_equal(
    out1$code,
    quote(beta_x_group[1:3] ~ forLoop(dnorm(0, sd = sd_x_group)))
  )

  out2 <- makeRandomPriorCode(quote(x|group), quote(beta_), NULL, dat, indexCreator=idxCreator)
  expect_equal(
    out2$code,
    quote({
    {
        re_sds_group[1] <- sd_group
        re_sds_group[2] <- sd_x_group
    }
    {
        Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
        U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 1:2], re_sds_group[1:2])
    }
    re_means_group[1:2] <- rep(0, 2)
    for (i_1 in 1:3) {
        B_group[i_1, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 1:2], prec_param = 0)
        beta_group[i_1] <- B_group[i_1, 1]
        beta_x_group[i_1] <- B_group[i_1, 2]
    }


    })
  )
})

test_that("removeExtraBrackets", {       
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  idxCreator <- nimble:::labelFunctionCreator("i")
  inp <- makeRandomPriorCode(quote(x|group), quote(beta_), NULL, dat, indexCreator=idxCreator)
  
  expect_equal(
    removeExtraBrackets(inp$code),
    quote({
    re_sds_group[1] <- sd_group
    re_sds_group[2] <- sd_x_group
    Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
    U_group[1:2, 1:2] <- uppertri_mult_diag(Ustar_group[1:2, 1:2], re_sds_group[1:2])
    re_means_group[1:2] <- rep(0, 2)
    for (i_1 in 1:3) {
        B_group[i_1, 1:2] ~ dmnorm(re_means_group[1:2], cholesky = U_group[1:2, 1:2], prec_param = 0)
        beta_group[i_1] <- B_group[i_1, 1]
        beta_x_group[i_1] <- B_group[i_1, 2]
    }
  })
 )
  
  # Test removing brackets inside for loops
  code <- nimbleCode({
  {
    alpha <- 1
    for (i in 1:n){
      {
        for (j in 1:k){
          z <- 1
          {
          y <- 1
          }
        }
      }
    }
  }
  })
  
  expect_equal(
    removeExtraBrackets(code),
    quote({
      alpha <- 1
      for (i in 1:n){
        for (j in 1:k){
          z <- 1
          y <- 1
        }
      }
    })
  )

})

test_that("processNestedRandomEffects", {
  dat <- list(group=factor(c("a","b","c")), group2=factor(c("d","e","f")))
  
  out1 <- processNestedRandomEffects(quote(1|group), dat)
  expect_equal(out1$barExp, quote(1|group))
  expect_equal(out1$constants, dat)

  out2 <- processNestedRandomEffects(quote(1|group:group2), dat)
  expect_equal(out2$barExp, quote(1|group_group2))
  dat2 <- dat
  dat2$group_group2 <- factor(c("a:d", "b:e", "c:f"))
  expect_equal(out2$constants, dat2)
  
  # Check the new factor is not duplicated
  out3 <- processNestedRandomEffects(quote(1|group:group2), dat2)
  expect_equal(out3$constants, dat2)

})

test_that("processBar", { 
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  out <- processBar(quote(1|group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), dat)
  expect_equal(out$formula, quote(group))
  expect_equal(
    out$code,
    quote({
      alpha_sd_group ~ dunif(0, 3)
      beta_group[1:3] ~ forLoop(dnorm(0, sd = alpha_sd_group))
    })
  )
  expect_equal(out$constants, dat)

})

test_that("processAllBars", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  out <- processAllBars(~(x||group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), dat)
  expect_equal(out$formula, quote(group + x:group))
  expect_equal(
    out$code,
    quote({
      alpha_sd_group ~ dunif(0, 3)
      beta_group[1:3] ~ forLoop(dnorm(0, sd = alpha_sd_group))
      alpha_sd_x_group ~ dunif(0, 3)
      beta_x_group[1:3] ~ forLoop(dnorm(0, sd = alpha_sd_x_group))
    })
  )
  expect_equal(out$constants, dat)

  expect_error(
    processAllBars(~(x||group) + (1|group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), dat)
  )
})
