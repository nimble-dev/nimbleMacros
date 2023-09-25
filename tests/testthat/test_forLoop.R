context("forLoop and related functions")

test_that("extractIndices", {
  expect_equal(
    extractIndices(quote(beta[1:10])),
    list(quote(1:10))
  )
  expect_equal(
    extractIndices(quote(beta[1,2:5,k])),
    list(quote(1), quote(2:5), quote(k))
  )
  #expect_equal(
  #  extractIndices(quote(beta[1,2:5,k] ~ dnorm(0, 3))),
  #  list(quote(1), quote(2:5), quote(k))
  #)
  expect_error(
    extractIndices(quote(beta ~ dnorm(0, 3)))
  )
})

test_that("isIndexRange", {
  expect_equal(
    isIndexRange(list(quote(1:10), quote(1), quote(k))),
    c(TRUE, FALSE, FALSE)
  )
  expect_equal(
    isIndexRange(list(quote(1))),
    c(FALSE)
  )
})

test_that("removeMacroCall", {
  expect_equal(
    removeMacroCall(quote(beta[1:10] ~ fakeMacro(alpha[1]))),
    quote(beta[1:10] ~ alpha[1])
  )
  expect_equal(
    removeMacroCall(quote(beta[1:10] ~ fakeMacro(dnorm, 0, 10))),
    quote(beta[1:10] ~ dnorm(0, 10))
  )
  expect_error(removeMacroCall(quote(beta[1:10])))
})

test_that("extractAllIndices", {
  expect_equal(
    extractAllIndices(quote(beta[1])),
    list(quote(1))
  )
  expect_equal(
    extractAllIndices(quote(beta[1:10,1,1:k])),
    list(quote(1:10), quote(1), quote(1:k))
  )
  expect_equal(
    extractAllIndices(quote(beta)),
    NULL
  )
  expect_equal(
    extractAllIndices(quote(dnorm(beta[1:k, 1:2], sigma))),
    list(quote(1:k), quote(1:2))
  )
  expect_equal(
    extractAllIndices(quote(dnorm(beta[1:k, 1:2], sigma[l]))),
    list(quote(1:k), quote(1:2), quote(l))
  )
})

test_that("removeIndexAdjustments", {
  idx <- quote(1:n)
  expect_equal(removeIndexAdjustments(idx), quote(1:n))
  idx <- quote(1:n - 1)
  expect_equal(removeIndexAdjustments(idx), quote(1:n))
  idx <- quote(1-1:n)
  expect_equal(removeIndexAdjustments(idx), quote(1:n))
  idx <- quote(1:n+2)
  expect_equal(removeIndexAdjustments(idx), quote(1:n))
})

test_that("hasMatchingIndexRanges", {
  expect_true(
    hasMatchingIndexRanges(quote(beta[1]), quote(dnorm(alpha[1])))
  )
  expect_true(
    hasMatchingIndexRanges(quote(beta[1:k]), quote(dnorm(alpha[1:k])))
  )
  expect_false(
    hasMatchingIndexRanges(quote(beta[1:k]), quote(dnorm(alpha[1:l])))
  )
  expect_true(
    hasMatchingIndexRanges(quote(beta[1:k, 2:3]), quote(dnorm(alpha[1:k])))
  )
  expect_false(
    hasMatchingIndexRanges(quote(beta[1:k, 2:3]), quote(dnorm(alpha[1:k, 3:4])))
  )
  expect_true(
    hasMatchingIndexRanges(quote(beta[1:n]), quote(dnorm(alpha[1:n-1])))
  )
})

test_that("hasAdjustment", {
  idx <- quote(1:n)
  expect_false(hasAdjustment(idx))
  idx <- quote(1:n+1)
  expect_true(hasAdjustment(idx))
  idx <- quote(1:n -1)
  expect_true(hasAdjustment(idx))
})

test_that("replaceIndex", {
  expect_equal(
    replaceIndex(quote(alpha[1:10,3]), quote(1:10), quote(k)),
    quote(alpha[k, 3])
  )
  expect_equal(
    replaceIndex(quote(alpha[3,1:k]), quote(1:k), quote(j)),
    quote(alpha[3,j])
  )
  expect_equal(
    replaceIndex(quote(alpha[3,1:k]), quote(1:l), quote(j)),
    quote(alpha[3,1:k])
  )
  expect_equal(
    replaceIndex(quote(alpha[3,1:k-1]), quote(1:k), quote(j)),
    quote(alpha[3,j-1])
  )
})

test_that("recursiveReplaceIndex", {
  expect_equal(
    recursiveReplaceIndex(quote(dnorm(alpha[1:10,3], sigma)), quote(1:10), quote(k)),
    quote(dnorm(alpha[k, 3], sigma))
  )
  expect_equal(
    recursiveReplaceIndex(quote(dnorm(alpha[3,1:k], sigma)), quote(1:k), quote(j)),
    quote(dnorm(alpha[3,j], sigma))
  )
  expect_equal(
    recursiveReplaceIndex(quote(dnorm(alpha[3,1:k], sigma)), quote(1:l), quote(j)),
    quote(dnorm(alpha[3,1:k], sigma))
  )
  expect_equal(
    recursiveReplaceIndex(quote(dnorm(alpha[1:10,3], sigma[1:10])), quote(1:10), quote(k)),
    quote(dnorm(alpha[k, 3], sigma[k]))
  )
  expect_equal(
    recursiveReplaceIndex(quote(1 - (1 - alpha[1:10,3])), quote(1:10), quote(k)),
    quote(1-(1-alpha[k, 3]))
  )
})

test_that("replaceDeclarationIndexRanges", {
  new_idx <- list(quote(i_), quote(j_), quote(k_))
  expect_equal(
    replaceDeclarationIndexRanges(
      quote(alpha[1:3, 1:k, 1] ~ dnorm(beta[1:k, 1:3], sigma)), new_idx),
    quote(alpha[i_, j_, 1] ~ dnorm(beta[j_,i_], sigma))
  )
  expect_equal(
    replaceDeclarationIndexRanges(
      quote(alpha[1:k] ~ dnorm(beta[1], sigma)), new_idx),
    quote(alpha[i_] ~ dnorm(beta[1], sigma))
  )
  expect_equal(
    replaceDeclarationIndexRanges(
      quote(alpha[1] ~ dnorm(beta[1], sigma)), new_idx),
    quote(alpha[1] ~ dnorm(beta[1], sigma))
  )
  expect_error(
    replaceDeclarationIndexRanges(
      quote(alpha[1:k] ~ dnorm(beta[1:l], sigma)), new_idx)
  )
  expect_error(
    replaceDeclarationIndexRanges(
      quote(alpha[1] ~ dnorm(beta[1:10], sigma)), new_idx)
  )
  expect_equal(
    replaceDeclarationIndexRanges(
      quote(alpha[1:10] ~ dnorm(beta[1:10], sigma[1:10])), new_idx),
    quote(alpha[i_] ~ dnorm(beta[i_], sigma[i_]))
  )
  expect_equal(
    replaceDeclarationIndexRanges(
      quote(alpha[1:10] ~ dnorm(beta[1:10], sigma[1])), new_idx),
    quote(alpha[i_] ~ dnorm(beta[i_], sigma[1]))
  )
})

test_that("forLoop", {
  comments_on <- nimbleOptions()$enableMacroComments
  nimbleOptions(enableMacroComments=FALSE)
  expect_equal(
    # macro
    nimble:::processModelMacros(nimbleCode({
      beta[1:10] ~ forLoop(dnorm(0, sd=10))
    }), modelInfo=list())$code,
    # reference
    nimbleCode({
      for (i_1 in 1:10){
        beta[i_1] ~ dnorm(0, sd = 10)
      }
    })  
  )

  expect_equal(
    # macro
    nimble:::processModelMacros(nimbleCode({
      beta[1:2,1:10,1] ~ forLoop(dnorm(0, sd=10))
    }), modelInfo=list())$code,
    # reference
    nimbleCode({
      for (i_1 in 1:2) {
        for (i_2 in 1:10) {
            beta[i_1, i_2, 1] ~ dnorm(0, sd = 10)
        }
      }
    })
  )

  expect_equal(
    # macro
    nimble:::processModelMacros(nimbleCode({
      sigma ~ forLoop(dunif(0,10))
    }), modelInfo=list())$code,
    # reference
    nimbleCode({
      sigma ~ dunif(0, 10)
    })
  )

  expect_equal(
    # macro
    nimble:::processModelMacros(nimbleCode({
      beta[1,2] ~ forLoop(dnorm(0, sd=10))
    }), modelInfo=list())$code,
    # reference
    nimbleCode({
      beta[1,2] ~ dnorm(0, sd=10)
    })
  )

  expect_equal(
    # macro
    nimble:::processModelMacros(nimbleCode({
      beta[1:10,1:k,1:l] ~ forLoop(dnorm(alpha[1:k, 1:10], sigma[1:l]))
    }), modelInfo=list())$code,
    # reference
    nimbleCode({
      for (i_1 in 1:10) {
        for (i_2 in 1:k) {
            for (i_3 in 1:l) {
                beta[i_1, i_2, i_3] ~ dnorm(alpha[i_2, i_1], sigma[i_3])
            }
        }
      }
    })
  )

  nimbleOptions(enableMacroComments=comments_on)
})
