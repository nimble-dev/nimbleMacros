#' Macro for fitting linear models, GLMs, and GLMMs
#'
#' This macro supports formula notation and arguments similar to R functions
#' such as lm(), glm(), and lmer()/glmer(). Currently only normal and Poisson
#' models are supported.
#'
#' @name nimbleLM
#' @author Ken Kellner
#'
#' @param formula An R formula, possibly with the parameters followed by 
#'  brackets containing indices. If there are no indices, the macro attempts
#'  to guess the correct indices from the context. Formulas can include
#'  random effects via lme4-style notation (e.g. ~ x + (1|group))
#' @family A description of the error distribution and link function to
#'  be used in the model. This can be a character string naming a family function, 
#'  a family function or the result of a call to a family function. See ?family 
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta. (so x becomes beta.x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param coefPrior BUGS code for prior on coefficients. Default is dnorm(0, sd=10).
#'  If this parameter is specified, the priors() macro will also be called.
#' @param sdPrior BUGS code for prior on dispersion parameters. Default is
#'  half-Cauchy T(dt(0, 0.1, 1), 0,). If this parameter is specified, the
#'  priors() macro will also be called.
#'
#' @examples
#' \donttest{
#' constants <- list(y = rnorm(10),
#'                   x = rnorm(10), 
#'                   x2 = factor(sample(letters[1:3], 10, replace=T)))
#' 
#' code <- nimbleCode({
#'   nimbleLM(y ~ x + x2)
#' })
#' nimble:::codeProcessModelMacros(code, constants)$code
#' 
#' # equivalent
#' code <- nimbleCode({
#'   y ~ nimbleLM(~ x + x2)
#' })
#' nimble:::codeProcessModelMacros(code, constants)$code
#' }
NULL

#' @export
nimbleLM <- list(process = function(code, .constants, .env){
  
  if(isAssignment(code)){
    RHS <- getRHS(code)
    LHS <- getLHS(code)
  } else {
    LHS <- code[[2]][[2]]
    code[[2]][[2]] <- NULL
    #code[[1]] <- NULL
    RHS <- code
  }
  
  # Guess LHS bracket index if it's missing
  if(hasBracket(LHS)){
    idx <- extractIndices(LHS)[[1]]
  } else {
    idx <- substitute(1:LEN, list(LEN=as.numeric(length(.constants[[deparse(LHS)]]))))
    LHS <- substitute(LHS[IDX], list(LHS=LHS, IDX=idx))
  }

  form <- RHS[[2]]
  family <- if(is.null(RHS$family)) quote(gaussian) else RHS$family
  family <- processFamily(family)
  link <- if(family$link == "identity") NULL else as.name(family$link)

  coefPrior <- if(is.null(RHS$coefPrior)) quote(dnorm(0, sd=100)) else RHS$coefPrior
  sdPrior <- if(is.null(RHS$sdPrior)) quote(dunif(0, 100)) else RHS$sdPrior
  coefPrefix <- if(is.null(RHS$coefPrefix)) quote(beta.) else RHS$coefPrefix
  sdPrefix <- RHS$sdPrefix

  if(is.null(sdPrefix)){
    sd_res <- quote(sd.residual)
  } else {
    sd_res <- str2lang(paste0(deparse(sdPrefix),"sd.residual"))
  }
  
  dataDec <- getDataDistCode(family$family, LHS, idx, sd_res)
  # FIXME: LHS par should not be fixed at mu
  LP <- substitute(mu[IDX] <- linPred(FORM, link=LINK, prefix=PREFIX),
                   list(IDX=idx, FORM=form, PREFIX=coefPrefix, LINK=link))
  LPprior <- substitute(PREFIX ~ priors(FORM, sdPrefix=SDPREFIX, coefPrior=COEFPRIOR, 
                                        sdPrior=SDPRIOR, modMatNames=TRUE),
                        list(PREFIX=coefPrefix, FORM=form, SDPREFIX=sdPrefix,
                             COEFPRIOR=coefPrior, SDPRIOR=sdPrior))
  out <- list(dataDec, LP, LPprior)

  if (family$family == "gaussian"){
    sigprior <- substitute(SDRES ~ SDPRIOR, list(SDPRIOR=sdPrior, SDRES=sd_res))
    out <- c(out, list(sigprior))
  }

  list(code=removeExtraBrackets(embedLinesInCurlyBrackets(out)), constants=.constants)
})
class(nimbleLM) <- 'model_macro'

# FIXME: Should eventually support more options
processFamily <- function(fam){
  if (is.character(fam)){
    fam <- do.call(fam, list())
  } else if(is.name(fam)){
    fam <- eval(fam)() 
  } else if(is.call(fam)){
    fam <- eval(fam)
  }
  
  if(is.null(fam$family)){
    stop("'family' not recognized")
  }
  stopifnot(fam$link %in% c("log", "identity"))
  fam
}

# FIXME: This should be more general
getDataDistCode <- function(family, response, idx, dispParName){
  if(family == "gaussian"){
    out <- substitute(DAT ~ forLoop(DIST(mu[IDX], sd=DISPPAR)),
                        list(DAT=response, DIST=quote(dnorm), IDX=idx,
                             DISPPAR=dispParName))
  } else if(family == "poisson"){
    out <- substitute(DAT ~ forLoop(DIST(mu[IDX])),
                      list(DAT=response, DIST=quote(dpois), IDX=idx))
  }
  out
}
