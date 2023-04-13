context("forLoop and related functions")

test_that("hasBracket", {
  expect_true(hasBracket(quote(beta[1])))
  expect_false(hasBracket(quote(beta)))
  expect_true(hasBracket(quote(alpha[beta[1]])))
  expect_true(hasBracket(quote(~x[1:n])))
  expect_false(hasBracket(quote(~x[1:n]), recursive=FALSE))
})

test_that("isAssignment", {
  expect_true(isAssignment(quote(alpha <- 1)))
  expect_false(isAssignment(quote(alpha(1))))
})

test_that("isAssignment", {
  expect_true(
    isAssignment(quote(alpha ~ dbern(omega)))
  )
  expect_true(
    isAssignment(quote(alpha <- beta[1]))
  )
  expect_false(
    isAssignment(quote(beta[1]))
  )
})

test_that("getLHS", {
  expect_equal(
    getLHS(quote(alpha ~ dbern(omega))),
    quote(alpha)
  )
  expect_equal(
    getLHS(quote(alpha <- beta[1])),
    quote(alpha)
  )
  expect_error(
    getLHS(quote(beta[1]))
  )
})

test_that("getRHS", {
  expect_equal(
    getRHS(quote(alpha ~ dbern(omega))),
    quote(dbern(omega))
  )
  expect_equal(
    getRHS(quote(alpha <- beta[1])),
    quote(beta[1])
  )
  expect_error(
    getRHS(quote(beta[1]))
  )
})

test_that("LHS<-", {
  code <- quote(beta[1] ~ dnorm(0, 10))
  LHS(code) <- quote(test)
  expect_equal(
    code,
    quote(test ~ dnorm(0,10))
  )
  expect_error(LHS(code) <- "test")
  code2 <- quote(alpha[1])
  expect_error(LHS(code2) <- quote(test))
})

test_that("RHS<-", {
  code <- quote(beta[1] ~ dnorm(0, 10))
  RHS(code) <- quote(test)
  expect_equal(
    code,
    quote(beta[1] ~ test)
  )
  expect_error(RHS(code) <- "test")
  code2 <- quote(alpha[1])
  expect_error(RHS(code2) <- quote(test))
})

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
  expect_equal(
    # macro
    nimble:::codeProcessModelMacros(nimbleCode({
      beta[1:10] ~ forLoop(dnorm(0, sd=10))
    })),
    # reference
    list(code=nimbleCode({
      for (i_ in 1:10){
        beta[i_] ~ dnorm(0, sd = 10)
      }
    }), constants=list())
  )

  expect_equal(
    # macro
    nimble:::codeProcessModelMacros(nimbleCode({
      beta[1:2,1:10,1] ~ forLoop(dnorm(0, sd=10))
    })),
    # reference
    list(code=nimbleCode({
      for (i_ in 1:2) {
        for (j_ in 1:10) {
            beta[i_, j_, 1] ~ dnorm(0, sd = 10)
        }
      }
    }), constants=list()
  ))

  expect_equal(
    # macro
    nimble:::codeProcessModelMacros(nimbleCode({
      sigma ~ forLoop(dunif(0,10))
    })),
    # reference
    list(code=nimbleCode({
      sigma ~ dunif(0, 10)
    }), constants=list())
  )

  expect_equal(
    # macro
    nimble:::codeProcessModelMacros(nimbleCode({
      beta[1,2] ~ forLoop(dnorm(0, sd=10))
    })),
    # reference
    list(code=nimbleCode({
      beta[1,2] ~ dnorm(0, sd=10)
    }), constants=list())
  )

  expect_equal(
    # macro
    nimble:::codeProcessModelMacros(nimbleCode({
      beta[1:10,1:k,1:l] ~ forLoop(dnorm(alpha[1:k, 1:10], sigma[1:l]))
    })),
    # reference
    list(code=nimbleCode({
      for (i_ in 1:10) {
        for (j_ in 1:k) {
            for (k_ in 1:l) {
                beta[i_, j_, k_] ~ dnorm(alpha[j_, i_], sigma[k_])
            }
        }
      }
    }), constants=list()
  )
  )

})
