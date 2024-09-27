context("random effects functions")

test_that("isBar", {
  expect_true(isBar(quote(1|group)))
  expect_false(isBar(quote(group)))
})

test_that("barToTerms", {  
  expect_equal(
    barToTerms(quote(1|group)),
    "group"
  )
  expect_equal(
    barToTerms(quote(x|group)),
    c("group", "x:group")
  )
  expect_equal(
    barToTerms(quote(x-1|group)),
    "x:group"
  )
  expect_equal(
    barToTerms(quote(x+0|group)),
    "x:group"
  )
  expect_equal(
    barToTerms(quote(x*y|group)),
    c("group", "x:group", "y:group", "x:y:group")
  )
  expect_equal(
    barToTerms(quote(1|group[1:n]), keep_idx = TRUE),
    "group[1:n]"
  )
  expect_equal(
    barToTerms(quote(x|group[1:n]), keep_idx = TRUE),
    c("group[1:n]", "x:group[1:n]")
  )
  expect_equal(
    barToTerms(quote(1|group[1:n]), keep_idx = FALSE),
    "group"
  )
  expect_equal(
    barToTerms(quote(x|group[1:n]), keep_idx = FALSE),
    c("group", "x:group")
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
  expect_equal(
    getRandomFactorName(quote(x|group2[1:n])),
    quote(group2)
  )
})

test_that("getHyperpriorNames", {
  
  modelInfo <- list(constants=list(group=factor(letters[1:3]),
                                   x = rnorm(3),
                                   z = factor(letters[4:6]), a = factor(letters[7:8]),
                                   w = factor(letters[9:10])))

  expect_equal(
    getHyperpriorNames(quote(1|group), modelInfo, quote(beta_)),
    list(list(quote(beta_sd_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(x|group), modelInfo, NULL),
    list(list(quote(sd_group)), list(quote(sd_x_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(x-1|group), modelInfo, NULL),
    list(list(quote(sd_x_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(x*y|group), modelInfo, NULL),
    list(list(quote(sd_group)), list(quote(sd_x_group)),
         list(quote(sd_y_group)), list(quote(sd_x_y_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(x|group[1:n]), modelInfo, NULL),
    list(list(quote(sd_group)), list(quote(sd_x_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(x[1:n]|group[1:n]), modelInfo, NULL),
    list(list(quote(sd_group)), list(quote(sd_x_group)))
  )

  # Factors
  expect_equal(
    getHyperpriorNames(quote(0+z|group), modelInfo, NULL),
    list(list(quote(sd_zd_group), quote(sd_ze_group), quote(sd_zf_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(z|group), modelInfo, NULL),
    list(list(quote(sd_group)), list(quote(sd_zd_group), quote(sd_ze_group), quote(sd_zf_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(0+z:x|group), modelInfo, NULL),
    list(list(quote(sd_zd_x_group), quote(sd_ze_x_group), quote(sd_zf_x_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(0+x:z|group), modelInfo, NULL),
    list(list(quote(sd_x_zd_group), quote(sd_x_ze_group), quote(sd_x_zf_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(0+a:z|group), modelInfo, NULL),
    list(list(quote(sd_ag_zd_group), quote(sd_ah_zd_group), quote(sd_ag_ze_group),
              quote(sd_ah_ze_group), quote(sd_ag_zf_group), quote(sd_ah_zf_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(0+z:a|group), modelInfo, NULL),
    list(list(quote(sd_zd_ag_group), quote(sd_ze_ag_group), quote(sd_zf_ag_group),
              quote(sd_zd_ah_group), quote(sd_ze_ah_group), quote(sd_zf_ah_group)))
  )
  expect_equal(
    getHyperpriorNames(quote(0+z:x|group), modelInfo, NULL),
    list(list(quote(sd_zd_x_group), quote(sd_ze_x_group), quote(sd_zf_x_group)))
  )

  hp <- getHyperpriorNames(quote(0+a:z:w|group), modelInfo, NULL)
  hp_string <- sapply(hp[[1]], safeDeparse)
  expect_equal(
    hp_string,
    c("sd_ag_zd_wi_group", "sd_ah_zd_wi_group", "sd_ag_ze_wi_group",
    "sd_ah_ze_wi_group", "sd_ag_zf_wi_group", "sd_ah_zf_wi_group",
    "sd_ag_zd_wj_group", "sd_ah_zd_wj_group", "sd_ag_ze_wj_group",
    "sd_ah_ze_wj_group", "sd_ag_zf_wj_group", "sd_ah_zf_wj_group"
    )
  )
})

test_that("makeHyperpriorCode", {
  modelInfo <- list(constants=list(group=factor(letters[1:3]),
                                   x = rnorm(3)))
  pr <- setPriors(sd = quote(dunif(0, 3)))
  expect_equal(
    makeHyperpriorCode(quote(1|group), modelInfo, quote(alpha_), pr),
    quote({
      alpha_sd_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x|group), modelInfo, NULL, pr),
    quote({
      sd_group ~ dunif(0, 3)
      sd_x_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x-1|group), modelInfo, NULL, pr),
    quote({
      sd_x_group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x*y|group), modelInfo, NULL, pr),
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
  dat2 <- list(group = matrix(c("a","b","a","b"), 2, 2), x = rnorm(3))
  expect_equal(
    numRandomFactorLevels(quote(1|group), dat2),
    2
  )
})

test_that("makeUncorrelatedRandomPrior", {
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3),
                                 z=factor(letters[4:5]), w=factor(letters[6:8])))
  expect_equal(
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta_), NULL, modInfo),
    quote({beta_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_group))})
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta_), quote(alpha_), modInfo),
    quote({beta_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = alpha_sd_group))})
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(x-1|group), quote(beta_), NULL, modInfo),
    quote({beta_x_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_group))})
  )
  # Not an uncorrelated random effect
  expect_error(
    makeUncorrelatedRandomPrior(quote(x|group), quote(beta_), NULL, modInfo)
  )

  # Factors
  expect_equal(
    makeUncorrelatedRandomPrior(quote(0+z|group), quote(beta_), NULL, modInfo),
    quote({
      beta_z_group[1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_zd_group))
      beta_z_group[2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_ze_group))
    })
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(0+z:x|group), quote(beta_), NULL, modInfo),
    quote({
      beta_z_x_group[1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_zd_x_group))
      beta_z_x_group[2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_ze_x_group))
    })
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(0+z:w|group), quote(beta_), NULL, modInfo),
    quote({
      beta_z_w_group[1, 1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_zd_wf_group))
      beta_z_w_group[2, 1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_ze_wf_group))
      beta_z_w_group[1, 2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_zd_wg_group))
      beta_z_w_group[2, 2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_ze_wg_group))
      beta_z_w_group[1, 3, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_zd_wh_group))
      beta_z_w_group[2, 3, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_ze_wh_group))
    })
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(0+w:z|group), quote(beta_), NULL, modInfo),
    quote({
      beta_w_z_group[1, 1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wf_zd_group))
      beta_w_z_group[2, 1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wg_zd_group))
      beta_w_z_group[3, 1, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wh_zd_group))
      beta_w_z_group[1, 2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wf_ze_group))
      beta_w_z_group[2, 2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wg_ze_group))
      beta_w_z_group[3, 2, 1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_wh_ze_group))
    })
  )
})

test_that("makeCorrelatedRandomPrior", {
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3), z=factor(letters[4:6])),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  out <- makeCorrelatedRandomPrior(quote(x|group), quote(beta_), NULL, modInfo, priorInfo=setPriors())
  expect_equal(
    out,
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

  # Test changing eta
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  pr <- setPriors()
  pr$eta <- 2
  out <- makeCorrelatedRandomPrior(quote(x|group), quote(beta_), NULL, modInfo, priorInfo=pr)
  expect_equal(
    out,
    quote({
    {
        re_sds_group[1] <- sd_group
        re_sds_group[2] <- sd_x_group
    }
    {
        Ustar_group[1:2, 1:2] ~ dlkj_corr_cholesky(2, 2)
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
    makeCorrelatedRandomPrior(quote(1|group), quote(beta_), NULL, modInfo, priorInfo=NULL)
  )

  expect_error(
    makeCorrelatedRandomPrior(quote(z|group), quote(beta_), NULL, modInfo, priorInfo=NULL)
  )
})

test_that("makeRandomPriorCode", {
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  out1 <- makeRandomPriorCode(quote(x+0|group), quote(beta_), NULL, modInfo, priorInfo=setPriors())
  expect_equal(
    out1,
    quote({beta_x_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = sd_x_group))})
  )

  out2 <- makeRandomPriorCode(quote(x|group), quote(beta_), NULL, modInfo, priorInfo=setPriors())
  expect_equal(
    out2,
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
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  inp <- makeRandomPriorCode(quote(x|group), quote(beta_), NULL, modInfo, priorInfo=setPriors())
  
  expect_equal(
    removeExtraBrackets(inp),
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

test_that("processBar", { 
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  pr <- setPriors(sd = quote(dunif(0, 3)))
  out <- processBar(quote(1|group), priorInfo=pr, coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), modInfo)
  expect_equal(out$terms, "group")
  expect_equal(
    out$code,
    quote({
      alpha_sd_group ~ dunif(0, 3)
      beta_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = alpha_sd_group))
    })
  )
  expect_equal(out$modelInfo$constants, modInfo$constants)

})

test_that("processAllBars", {
  modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
                  indexCreator=nimble:::labelFunctionCreator("i"))
  pr <- setPriors(sd = quote(dunif(0, 3)))
  out <- processAllBars(~(x||group), priors=pr, coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), modInfo)
  expect_equal(out$terms, c("group", "x:group"))
  expect_equal(
    out$code,
    quote({
      alpha_sd_group ~ dunif(0, 3)
      beta_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = alpha_sd_group))
      alpha_sd_x_group ~ dunif(0, 3)
      beta_x_group[1:3] ~ nimbleMacros::FORLOOP(dnorm(0, sd = alpha_sd_x_group))
    })
  )
  expect_equal(out$modelInfo$constants, modInfo$constants)

  expect_error(
    processAllBars(~(x||group) + (1|group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta_), 
                    sdPrefix=quote(alpha_), modInfo)
  )
})

test_that("centeredFormulaDropTerms", {
  expect_equal(centeredFormulaDropTerms(~x + (1|group), quote(group)), "1")
  expect_equal(centeredFormulaDropTerms(~x + (x|group), quote(group)), c("1", "x"))
  expect_equal(centeredFormulaDropTerms(~x + (x||group), quote(group)), c("1", "x"))
  expect_equal(centeredFormulaDropTerms(~x + (1|group) + (x+0|group), quote(group)), c("1","x"))
  expect_equal(centeredFormulaDropTerms(~x + (x+0|group) + (y+0|group), quote(group)), c("x","y"))
  expect_equal(centeredFormulaDropTerms(~x + (x+y|group), quote(group)), c("1","x","y"))
  expect_equal(centeredFormulaDropTerms(~x + (x+y-1|group), quote(group)), c("x","y"))
  expect_equal(centeredFormulaDropTerms(~x + (x*y|group), quote(group)), c("1","x","y","x:y"))
  expect_equal(centeredFormulaDropTerms(~x + (x+0|group), quote(group)), c("x"))
  expect_equal(centeredFormulaDropTerms(~x + (x+0|group), quote(test)), NULL)
  expect_equal(centeredFormulaDropTerms(~x + (x+0|group), NULL), NULL)
  expect_equal(centeredFormulaDropTerms(~x + (x|group) + (y|test), quote(test)), c("1","y"))
})
