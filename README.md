# nimbleMacros

A plug-in to the 'nimble' R package that provides various macros usable in NIMBLE's dialect of BUGS.

``` r
library(nimble)
library(nimbleMacros)

data(longley)

nimble_data <- list(gnp=longley$GNP, employed=longley$Employed,
                    n=length(longley$Employed))

code <- nimbleCode({

  employed[1:n] ~ buildLoop(dnorm(mu[1:n], sd = sigma))
  mu[1:n] <- buildLoop(beta[1] + beta[2]*gnp[1:n])

  beta[1:2] ~ buildLoop(dnorm(0, sd=1000))
  sigma ~ dunif(0, 1000)

})

out <- nimbleMCMC(code, constants=nimble_data, nchains=3, niter=1000, nburnin=500,
                  summary = TRUE)
```
