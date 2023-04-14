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
    getHyperpriorNames(quote(1|group), quote(beta.)),
    list(beta.sd.group=quote(beta.sd.group))
  )
  expect_equal(
    getHyperpriorNames(quote(x|group), NULL),
    list(sd.group=quote(sd.group), sd.x.group=quote(sd.x.group))
  )
  expect_equal(
    getHyperpriorNames(quote(x-1|group), NULL),
    list(sd.x.group=quote(sd.x.group))
  )
  expect_equal(
    getHyperpriorNames(quote(x*y|group), NULL),
    list(sd.group=quote(sd.group), sd.x.group=quote(sd.x.group),
         sd.y.group=quote(sd.y.group), sd.x.y.group=quote(sd.x.y.group))
  )
})

test_that("makeHyperpriorCode", {  
  expect_equal(
    makeHyperpriorCode(quote(1|group), quote(alpha.), quote(dunif(0, 3))),
    quote({
      alpha.sd.group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x|group), NULL, quote(dunif(0, 3))),
    quote({
      sd.group ~ dunif(0, 3)
      sd.x.group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x-1|group), NULL, quote(dunif(0, 3))),
    quote({
      sd.x.group ~ dunif(0, 3)
    })
  )
  expect_equal(
    makeHyperpriorCode(quote(x*y|group), NULL, quote(dunif(0, 3))),
    quote({
      sd.group ~ dunif(0, 3)
      sd.x.group ~ dunif(0, 3)
      sd.y.group ~ dunif(0, 3)
      sd.x.y.group ~ dunif(0, 3)
    })
  )
})

test_that("makeRandomParNames", {  
  expect_equal(
    makeRandomParNames(quote(1|group), quote(beta.)),
    list(beta.group=quote(beta.group))
  )
  expect_equal(
    makeRandomParNames(quote(x|group), quote(beta.)),
    list(beta.group=quote(beta.group), beta.x.group=quote(beta.x.group))
  )
  expect_equal(
    makeRandomParNames(quote(x-1|group), quote(beta.)),
    list(beta.x.group=quote(beta.x.group))
  )
  expect_equal(
    makeRandomParNames(quote(x*y|group), quote(beta.)),
    list(beta.group=quote(beta.group), beta.x.group=quote(beta.x.group),
         beta.y.group=quote(beta.y.group), beta.x.y.group=quote(beta.x.y.group))
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
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta.), NULL, dat),
    quote(beta.group[1:3] ~ forLoop(dnorm(0, sd = sd.group)))
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(1|group), quote(beta.), quote(alpha.), dat),
    quote(beta.group[1:3] ~ forLoop(dnorm(0, sd = alpha.sd.group)))
  )
  expect_equal(
    makeUncorrelatedRandomPrior(quote(x-1|group), quote(beta.), NULL, dat),
    quote(beta.x.group[1:3] ~ forLoop(dnorm(0, sd = sd.x.group)))
  )
  # Not an uncorrelated random effect
  expect_error(
    makeUncorrelatedRandomPrior(quote(x|group), quote(beta.), NULL, dat)
  )
})

test_that("makeCorrelatedRandomPrior", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  out <- makeCorrelatedRandomPrior(quote(x|group), quote(beta.), NULL, dat)
  expect_equal(
    out$code,
    quote({
    {
        re.sds.group[1] <- sd.group
        re.sds.group[2] <- sd.x.group
    }
    {
        Ustar.group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
        U.group[1:2, 1:2] <- uppertri_mult_diag(Ustar.group[1:2, 1:2], re.sds.group[1:2])
    }
    re.means.group[1:2] <- forLoop(0)
    for (i_ in 1:3) {
        B.group[i_, 1:2] ~ dmnorm(re.means.group[1:2], cholesky = U.group[1:2, 1:2], prec_param = 0)
        beta.group[i_] <- B.group[i_, 1]
        beta.x.group[i_] <- B.group[i_, 2]
    }


    })
  )

  expect_error(
    makeCorrelatedRandomPrior(quote(1|group), quote(beta.), NULL, dat)
  )
})

test_that("makeRandomPriorCode", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))

  out1 <- makeRandomPriorCode(quote(x+0|group), quote(beta.), NULL, dat)
  expect_equal(
    out1$code,
    quote(beta.x.group[1:3] ~ forLoop(dnorm(0, sd = sd.x.group)))
  )

  out2 <- makeRandomPriorCode(quote(x|group), quote(beta.), NULL, dat)
  expect_equal(
    out2$code,
    quote({
    {
        re.sds.group[1] <- sd.group
        re.sds.group[2] <- sd.x.group
    }
    {
        Ustar.group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
        U.group[1:2, 1:2] <- uppertri_mult_diag(Ustar.group[1:2, 1:2], re.sds.group[1:2])
    }
    re.means.group[1:2] <- forLoop(0)
    for (i_ in 1:3) {
        B.group[i_, 1:2] ~ dmnorm(re.means.group[1:2], cholesky = U.group[1:2, 1:2], prec_param = 0)
        beta.group[i_] <- B.group[i_, 1]
        beta.x.group[i_] <- B.group[i_, 2]
    }


    })
  )
})

test_that("removeExtraBrackets", {       
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  inp <- makeRandomPriorCode(quote(x|group), quote(beta.), NULL, dat)
  
  expect_equal(
    removeExtraBrackets(inp$code),
    quote({
    re.sds.group[1] <- sd.group
    re.sds.group[2] <- sd.x.group
    Ustar.group[1:2, 1:2] ~ dlkj_corr_cholesky(1.3, 2)
    U.group[1:2, 1:2] <- uppertri_mult_diag(Ustar.group[1:2, 1:2], re.sds.group[1:2])
    re.means.group[1:2] <- forLoop(0)
    for (i_ in 1:3) {
        B.group[i_, 1:2] ~ dmnorm(re.means.group[1:2], cholesky = U.group[1:2, 1:2], prec_param = 0)
        beta.group[i_] <- B.group[i_, 1]
        beta.x.group[i_] <- B.group[i_, 2]
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
  out <- processBar(quote(1|group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta.), 
                    sdPrefix=quote(alpha.), dat)
  expect_equal(out$formula, quote(group))
  expect_equal(
    out$code,
    quote({
      alpha.sd.group ~ dunif(0, 3)
      beta.group[1:3] ~ forLoop(dnorm(0, sd = alpha.sd.group))
    })
  )
  expect_equal(out$constants, dat)

})

test_that("processAllBars", {
  dat <- list(group=factor(c("a","b","c")), x=rnorm(3))
  out <- processAllBars(~(x||group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta.), 
                    sdPrefix=quote(alpha.), dat)
  expect_equal(out$formula, quote(group + x:group))
  expect_equal(
    out$code,
    quote({
      alpha.sd.group ~ dunif(0, 3)
      beta.group[1:3] ~ forLoop(dnorm(0, sd = alpha.sd.group))
      alpha.sd.x.group ~ dunif(0, 3)
      beta.x.group[1:3] ~ forLoop(dnorm(0, sd = alpha.sd.x.group))
    })
  )
  expect_equal(out$constants, dat)

  expect_error(
    processAllBars(~(x||group) + (1|group), sdPrior=quote(dunif(0, 3)), coefPrefix=quote(beta.), 
                    sdPrefix=quote(alpha.), dat)
  )
})
