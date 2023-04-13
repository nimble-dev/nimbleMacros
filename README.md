# nimbleMacros

A plug-in to the ‘nimble’ R package that provides various macros usable
in NIMBLE’s dialect of BUGS. Currently available macros are focused on
fitting linear models, GLMs, and GLMMs. For example, a simple linear
regression:

``` r
library(nimbleMacros)
```

``` r
data(longley)

nimble_data <- list(GNP=longley$GNP, Employed=longley$Employed)

code <- nimbleCode({
  nimbleLM(Employed ~ GNP)
})

# View expanded code
nimble:::codeProcessModelMacros(code, nimble_data)$code
```

    ## {
    ##     {
    ##         for (i_ in 1:16) {
    ##             Employed[i_] ~ dnorm(mu[i_], sd = sd.residual)
    ##         }
    ##         for (i_ in 1:16) {
    ##             mu[i_] <- beta.Intercept + beta.GNP * GNP[i_]
    ##         }
    ##         {
    ##             beta.Intercept ~ dnorm(0, sd = 100)
    ##             beta.GNP ~ dnorm(0, sd = 100)
    ##         }
    ##         sd.residual ~ dunif(0, 100)
    ##     }
    ## }

``` r
# Fit model
out <- nimbleMCMC(code, constants=nimble_data, nchains=3, niter=1000, nburnin=500,
                  summary=TRUE, progressBar=FALSE)
```

    ## Defining model

    ##   [Note] Using 'Employed' (given within 'constants') as data.

    ## Building model

    ## Setting data and initial values

    ## Running calculate on model
    ##   [Note] Any error reports that follow may simply reflect missing values in model variables.

    ## Checking model sizes and dimensions

    ##   [Note] This model is not fully initialized. This is not an error.
    ##          To see which variables are not initialized, use model$initializeInfo().
    ##          For more information on model initialization, see help(modelInitialization).

    ## Checking model calculations

    ## [Note] NAs were detected in model variables: beta.Intercept, logProb_beta.Intercept, beta.GNP, logProb_beta.GNP, sd.residual, logProb_sd.residual, mu, logProb_Employed.

    ## Compiling
    ##   [Note] This may take a minute.
    ##   [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.

    ## running chain 1...

    ## running chain 2...

    ## running chain 3...

``` r
round(out$summary$all.chains, 3)
```

    ##                  Mean Median St.Dev. 95%CI_low 95%CI_upp
    ## beta.GNP        0.035  0.035   0.002     0.031     0.039
    ## beta.Intercept 51.859 51.792   0.804    50.314    53.480
    ## sd.residual     0.729  0.700   0.152     0.505     1.110

``` r
# Compare to lm
summary(lm(Employed ~ GNP, data=longley))
```

    ## 
    ## Call:
    ## lm(formula = Employed ~ GNP, data = longley)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.77958 -0.55440 -0.00944  0.34361  1.44594 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 51.843590   0.681372   76.09  < 2e-16 ***
    ## GNP          0.034752   0.001706   20.37 8.36e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6566 on 14 degrees of freedom
    ## Multiple R-squared:  0.9674, Adjusted R-squared:  0.965 
    ## F-statistic: 415.1 on 1 and 14 DF,  p-value: 8.363e-12
