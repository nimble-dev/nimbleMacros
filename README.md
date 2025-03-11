# nimbleMacros

[![R build status](https://github.com/nimble-dev/nimbleMacros/workflows/R-CMD-check/badge.svg)](https://github.com/nimble-dev/nimbleMacros/actions)
[![CRAN](https://www.r-pkg.org/badges/version/nimbleMacros)](https://CRAN.R-project.org/package=nimbleMacros)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14975578.svg)](https://doi.org/10.5281/zenodo.14975578)

`nimbleMacros` is an R package that extends [NIMBLE](https://r-nimble.org/), an R package for hierarchical statistical modeling.
The package provides a set of model macros, which are special code chunks that NIMBLE automatically expands into larger blocks of valid NIMBLE model code.

For example, the following NIMBLE model code contains a macro called `LINPRED`, and the objective is to create a linear predictor containing an intercept and a slope for one covariate, `x`.

```r
code <- nimbleCode({
  mu[1:N] <- LINPRED(~x)
})
```

Given particular data and constants, NIMBLE will expand this macro into the following code:

```r
{
  for (i_1 in 1:N){
    mu[i_1] <- beta_Intercept + beta_x * x[i_1]
  }
}
```

Provided macros include:

* `LM`: A macro to generate code for complete linear, generalized linear, and generalized linear mixed models. The macro uses the same syntax as familiar R functions such as `lm`, `glm`, `glmer`, and `glmmTMB`.
* `LINPRED`: A macro to generate code for linear predictors using a formula-based syntax, including handling of continuous and categorical covariates and random effects.
* `LINPRED_PRIORS`: : A macro to generate code for prior specifications using a formula-based syntax, including handling of continuous and categorical covariates and random effects.
* `FORLOOP`: A macro generating a `for` loop or nested set of `for` loops based on a bracket and index based syntax.

See the [package vignette](https://cran.r-project.org/web/packages/nimbleMacros/vignettes/nimbleMacros.html) for more details.
