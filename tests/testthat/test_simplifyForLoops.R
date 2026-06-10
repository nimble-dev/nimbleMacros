context("simplifyForLoops")

skip_on_cran()

test_that("Nothing is done when there are no for loops", {
  test <- nimbleCode({
    x <- 1
    y <- 1
  })

  out <- simplifyForLoops(test) 

  expect_equal(out, test)
})

test_that("Basic for loop collapsing", {

  test <- nimbleCode({
    for (i in 1:3){
      x[i] <- y[i] + 1
    }

    for (j in 1:3){
      x2[j] <- y2[j] + 1
      x3[j] <- y3[j] + 1
    }
  })

  out <- simplifyForLoops(test)

  expect_equal(out,
    quote({
      for (i in 1:3){
        x[i] <- y[i] + 1
        x2[i] <- y2[i] + 1
        x3[i] <- y3[i] + 1
      }
    })
  )
})

test_that("More complex case of an occupancy model", {

  nimbleOptions(enableMacroComments = FALSE)
  occ <- nimbleCode({
    psi[1:nsites] <- LINPRED(~scale(x[1:nsites]), link=logit, coefPrefix=state_)
    p[1:nsites, 1:noccs] <- LINPRED(~x[1:nsites] + x2[1:nsites, 1:noccs], link=logit, coefPrefix=det_)

    z[1:nsites] ~ FORLOOP(dbern(psi[1:nsites]))
    y[1:nsites, 1:noccs] ~ FORLOOP(dbern(p[1:nsites, 1:noccs]*z[1:nsites]))
  })

  const <- list(nsites=10, noccs=3, y=matrix(0, 10, 3), x=rnorm(10),
                x2=matrix(rnorm(30),10,3))

  mod <- nimbleModel(occ, constants=const)
  
  out <- simplifyForLoops(mod$getCode())
  
  expect_equal(out,
    quote({
    for (i in 1:nsites) {
        logit(psi[i]) <- state_Intercept + state_x_scaled * x_scaled[i]
        for (j in 1:noccs) {
            logit(p[i, j]) <- det_Intercept + det_x * x[i] + det_x2 * x2[i, j]
            y[i, j] ~ dbern(p[i, j] * z[i])
        }
        z[i] ~ dbern(psi[i])
    }
    state_Intercept ~ dnorm(0, sd = 1000)
    state_x_scaled ~ dnorm(0, sd = 1000)
    det_Intercept ~ dnorm(0, sd = 1000)
    det_x ~ dnorm(0, sd = 1000)
    det_x2 ~ dnorm(0, sd = 1000)
    })
  )

  # Change indices
  out <- simplifyForLoops(mod$getCode(), new_indices=list(quote(f), quote(g)))
  
  expect_equal(out,
    quote({
    for (f in 1:nsites) {
        logit(psi[f]) <- state_Intercept + state_x_scaled * x_scaled[f]
        for (g in 1:noccs) {
            logit(p[f, g]) <- det_Intercept + det_x * x[f] + det_x2 * x2[f, g]
            y[f, g] ~ dbern(p[f, g] * z[f])
        }
        z[f] ~ dbern(psi[f])
    }
    state_Intercept ~ dnorm(0, sd = 1000)
    state_x_scaled ~ dnorm(0, sd = 1000)
    det_Intercept ~ dnorm(0, sd = 1000)
    det_x ~ dnorm(0, sd = 1000)
    det_x2 ~ dnorm(0, sd = 1000)
    })
  )

  # Don't change indices
  out <- simplifyForLoops(mod$getCode(), new_indices=FALSE)
  
  expect_equal(out,
    quote({
    for (i_1 in 1:nsites) {
        logit(psi[i_1]) <- state_Intercept + state_x_scaled * 
            x_scaled[i_1]
        for (i_3 in 1:noccs) {
            logit(p[i_1, i_3]) <- det_Intercept + det_x * x[i_1] + 
                det_x2 * x2[i_1, i_3]
            y[i_1, i_3] ~ dbern(p[i_1, i_3] * z[i_1])
        }
        z[i_1] ~ dbern(psi[i_1])
    }
    state_Intercept ~ dnorm(0, sd = 1000)
    state_x_scaled ~ dnorm(0, sd = 1000)
    det_Intercept ~ dnorm(0, sd = 1000)
    det_x ~ dnorm(0, sd = 1000)
    det_x2 ~ dnorm(0, sd = 1000)
    })
  )

  # Error when not enough indices are provided
  expect_error(simplifyForLoops(mod$getCode(), new_indices=list(quote(i))),
               "Not enough new indices provided")
})

test_that("Blank index slot is ignored", {
  # e.g. in x[1:n,] we should not add a new index letter for the ,]
  # presumably this would occur when you are planning to add the dimensions
  # to the dimensions argument manually

  test <- nimbleCode({
    for (i in 1:3){
      x[i] <- y[i] + 1
    }

    for (j in 1:3){
      z[j,] <- 0
    }

  })

  out <- simplifyForLoops(test)

  expect_equal(out,
    quote({
      for (i in 1:3){
        x[i] <- y[i] + 1
        z[i,] <- 0
      }
    })
  )
})

test_that("Remaining index ranges are ignored", {
  # Index ranges could remain in loop code based on use of ignore argument
  # make sure simplifyForLoops doesn't replace these with a new index
  test <- nimbleCode({
    for (i_1 in 1:n){
      for (i_2 in 1:2){
        y[i_1,i_2] <- sum(x[i_1,i_2,1:m])
      }
    }

    for(i_1 in 1:n){
      z[i_1] <- sum(a[1:3])
    }
  })

  out <- simplifyForLoops(test)

  expect_equal(out,
    quote({
    for (i in 1:n) {
        for (j in 1:2) {
            y[i, j] <- sum(x[i, j, 1:m])
        }
        z[i] <- sum(a[1:3])
    }
    })
  )
})
