% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LINPRED.R
\name{LINPRED}
\alias{LINPRED}
\title{Macro to build code for linear predictor from R formula}
\arguments{
\item{formula}{An R formula, possibly with the parameters followed by 
brackets containing indices. If there are no indices, the macro attempts
to guess the correct indices from the context. The formula must be 
right-hand side only (e.g. \code{~x}). This must always be the first argument supplied
to \code{LINPRED}.}

\item{link}{A link function that will be applied to the 
left-hand-side (the response) in the final linear predictor. Default is none.}

\item{coefPrefix}{All model coefficient names will begin with this prefix.
default is \code{"beta_"} (so 'x' becomes 'beta_x', etc.)}

\item{sdPrefix}{All dispersion parameters will begin with this prefix.
Default is no prefix.}

\item{priors}{Prior specifications, generated with \code{setPrior()}}

\item{modelMatrixNames}{Logical indicating if parameters should be named so they match the
names one would get from R's \code{model.matrix}. Default is \code{FALSE}.}

\item{noncentered}{Logical indicating whether to use noncentered parameterization
for random effects. Default is \code{FALSE}. Under the noncentered parameterization, 
random effects have a standard normal prior (\code{beta_x_raw ~ dnorm(0, sd = 1)}) 
and are then scaled by the hyperparameters (mean and SD), i.e., 
\code{beta_x = mu_beta + sd_beta * beta_x_raw}. This parameterization can improve 
mixing in some cases.}

\item{centerVar}{Grouping variable (covariate) to 'center' the random effects on.
By default (when NULL), random effects come from normal distributions with mean 0 
as with \code{lme4}. For example, for random intercepts by grouping variable \code{x},
the linear predictor would be \code{beta_Intercept + beta_x[x[i]]} and the
prior for the random effects would be \code{beta_x ~ dnorm(0, sd_x)}. When
\code{centerVar = x}, the linear predictor would be \code{beta_x[x[i]]}
and the random effect prior would be \code{beta_x ~ dnorm(beta_Intercept, sd = sd_x)}.
That is, the mean of the random effects is now \code{beta_Intercept}.
These two formulations should yield the same results. Note that this option
is unrelated to the \code{noncentered} argument despite the similar names.}

\item{modelInfo}{Used internally by nimbleMacros; a list of model information such as constants and dimensions}

\item{.env}{Used internally by nimbleMacros; the environment where the model was created}
}
\value{
NIMBLE code for the linear predictor specified by the formula, and optionally the associated priors.
}
\description{
Converts an R formula into corresponding code for a linear predictor in NIMBLE model code.
Options are available to specify a link function and to also generate
code for priors corresponding to the parameters in the linear predictor.
}
\examples{
constants <- list(x = rnorm(3), x2 = factor(letters[1:3]))
code <- nimbleCode({
  mu[1:3] <- LINPRED(~x + x2)
})

mod <- nimbleModel(code, constants = constants)
mod$getCode()
}
\author{
Ken Kellner
}
