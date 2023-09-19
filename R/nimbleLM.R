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
#' @param family A description of the error distribution and link function to
#'  be used in the model. This can be a character string naming a family function, 
#'  a family function or the result of a call to a family function. See ?family 
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta_ (so x becomes beta_x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priorSettings List of prior specifications, should be generated using 
#'  setPriors()
#'
#' @examples
#' constants <- list(y = rnorm(10),
#'                   x = rnorm(10), 
#'                   x2 = factor(sample(letters[1:3], 10, replace=TRUE)))
#'  
#' code <- nimbleCode({
#'    nimbleLM(y ~ x + x2)
#' })
#' 
#' mod <- nimbleModel(code, constants=constants)
#' mod$getCode()
NULL

#' @export
nimbleLM <- list(process = function(code, modelInfo, .env){
  
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
    idx <- substitute(1:LEN, list(LEN=as.numeric(length(modelInfo$constants[[deparse(LHS)]]))))
    LHS <- substitute(LHS[IDX], list(LHS=LHS, IDX=idx))
  }

  form <- RHS[[2]]
  family <- if(is.null(RHS$family)) quote(gaussian) else RHS$family
  family <- processFamily(family)
  link <- if(family$link == "identity") NULL else as.name(family$link)
  
  priorSettings <- if(is.null(RHS$priorSettings)) quote(setPriors()) else RHS$priorSettings
  coefPrefix <- if(is.null(RHS$coefPrefix)) quote(beta_) else RHS$coefPrefix
  sdPrefix <- RHS$sdPrefix

  if(is.null(sdPrefix)){
    sd_res <- quote(sd_residual)
  } else {
    sd_res <- str2lang(paste0(deparse(sdPrefix),"sd_residual"))
  }
  
  dataDec <- getDataDistCode(family$family, LHS, idx, sd_res)
  # FIXME: LHS par should not be fixed at mu
  LP <- substitute(mu_[IDX] <- linPred(FORM, link=LINK, coefPrefix=COEFPREFIX,
                                       sdPrefix=SDPREFIX, priorSettings=PRIORS),
                   list(IDX=idx, FORM=form, COEFPREFIX=coefPrefix, SDPREFIX=sdPrefix,
                        PRIORS=priorSettings, LINK=link))
  pars_added <- list(quote(mu_))
  out <- list(dataDec, LP)

  if (family$family == "gaussian"){
    priorSettings <- eval(priorSettings, envir=.env)
    sdPrior <- matchPrior(sd_res, "sd", priorSettings = priorSettings) 
    sigprior <- substitute(SDRES ~ SDPRIOR, list(SDPRIOR=sdPrior, SDRES=sd_res))
    pars_added <- c(pars_added, list(sd_res))
    out <- c(out, list(sigprior))
  }

  list(code=removeExtraBrackets(embedLinesInCurlyBrackets(out)),
       modelInfo = modelInfo)
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
    out <- substitute(DAT ~ forLoop(DIST(mu_[IDX], sd=DISPPAR)),
                        list(DAT=response, DIST=quote(dnorm), IDX=idx,
                             DISPPAR=dispParName))
  } else if(family == "poisson"){
    out <- substitute(DAT ~ forLoop(DIST(mu_[IDX])),
                      list(DAT=response, DIST=quote(dpois), IDX=idx))
  }
  out
}
