context("random effects functions")

test_that("isBar", {
  expect_true(isBar(quote(1|group)))
  expect_false(isBar(quote(group)))
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

#test_that("getHyperpriorNames", {
#  
#  modelInfo <- list(constants=list(group=factor(letters[1:3]),
#                                   x = rnorm(3),
#                                   y = rnorm(3),
#                                   n = 3,
#                                   z = factor(letters[4:6]), a = factor(letters[7:8]),
#                                   w = factor(letters[9:10])))
#
#  form_info <- processFormula(~(1|group), NULL, modelInfo)
#
#  expect_equal(
#    getHyperpriorNames(quote(1|group), modelInfo, form_info, quote(beta_)),
#    list(list(quote(beta_sd_group)))
#  )
#
#  form_info <- processFormula(~(x|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(x|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_group)), list(quote(sd_group_x)))
#  )
#
#  form_info <- processFormula(~(x-1|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(x-1|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_x_group)))
#  )
#
#  form_info <- processFormula(~(x*y|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(x*y|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_group)), list(quote(sd_group_x)),
#         list(quote(sd_group_y)), list(quote(sd_group_x_y)))
#  )
#
#  form_info <- processFormula(~(x|group[1:n]), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(x|group[1:n]), modelInfo, form_info, NULL),
#    list(list(quote(sd_group)), list(quote(sd_group_x)))
#  )
#
#  form_info <- processFormula(~(x[1:n]|group[1:n]), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(x[1:n]|group[1:n]), modelInfo, form_info, NULL),
#    list(list(quote(sd_group)), list(quote(sd_group_x)))
#  )
#
#  # Factors
#
#  form_info <- processFormula(~(0+z|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+z|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_zd_group), quote(sd_ze_group), quote(sd_zf_group)))
#  )
#  form_info <- processFormula(~(z|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(z|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_group)), list(quote(sd_group_zd), quote(sd_group_ze), quote(sd_group_zf)))
#  )
#
#  form_info <- processFormula(~(0+z:x|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+z:x|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_zd_x_group), quote(sd_ze_x_group), quote(sd_zf_x_group)))
#  )
#
#  form_info <- processFormula(~(0+x:z|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+x:z|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_x_zd_group), quote(sd_x_ze_group), quote(sd_x_zf_group)))
#  )
#
#  form_info <- processFormula(~(0+a:z|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+a:z|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_ag_zd_group), quote(sd_ah_zd_group), quote(sd_ag_ze_group),
#              quote(sd_ah_ze_group), quote(sd_ag_zf_group), quote(sd_ah_zf_group)))
#  )
#
#  form_info <- processFormula(~(0+z:a|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+z:a|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_zd_ag_group), quote(sd_ze_ag_group), quote(sd_zf_ag_group),
#              quote(sd_zd_ah_group), quote(sd_ze_ah_group), quote(sd_zf_ah_group)))
#  )
#
#  form_info <- processFormula(~(0+z:x|group), NULL, modelInfo)
#  expect_equal(
#    getHyperpriorNames(quote(0+z:x|group), modelInfo, form_info, NULL),
#    list(list(quote(sd_zd_x_group), quote(sd_ze_x_group), quote(sd_zf_x_group)))
#  )
#
#  form_info <- processFormula(~(0+a:z:w|group), NULL, modelInfo)
#  hp <- getHyperpriorNames(quote(0+a:z:w|group), modelInfo, form_info, NULL)
#  hp_string <- sapply(hp[[1]], safeDeparse)
#  expect_equal(
#    hp_string,
#    c("sd_ag_zd_wi_group", "sd_ah_zd_wi_group", "sd_ag_ze_wi_group",
#    "sd_ah_ze_wi_group", "sd_ag_zf_wi_group", "sd_ah_zf_wi_group",
#    "sd_ag_zd_wj_group", "sd_ah_zd_wj_group", "sd_ag_ze_wj_group",
#    "sd_ah_ze_wj_group", "sd_ag_zf_wj_group", "sd_ah_zf_wj_group"
#    )
#  )
#})

#test_that("getPriors", {

  #modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3),
  #                               z=factor(letters[4:5]), w=factor(letters[6:8])))
  # ~(1|group)  with sdPrefix change
  # ~(x-1|group)
  # ~(0+z|group)
  # ~(0+z:x|group)
  # ~(0+z:w|group)
  # ~(0+w:z|group)

  #modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3), z=factor(letters[4:6])),
  #                indexCreator=nimble:::labelFunctionCreator("i"))

  # ~(x|group)

  #modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
  #                indexCreator=nimble:::labelFunctionCreator("i"))
  # setPriors(eta = 2)
  # ~(x|group)


  #modInfo <- list(constants=list(group=factor(c("a","b","c")), x=rnorm(3)),
  #                indexCreator=nimble:::labelFunctionCreator("i"))
  # ~(x+0|group)

#})


test_that("removeExtraBrackets", {       
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
