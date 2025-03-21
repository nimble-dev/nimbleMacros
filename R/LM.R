#' Macro for fitting linear models, GLMs, and GLMMs
#'
#' This macro generates code for LMs, GLMs, and GLMMs using formula notation 
#' and arguments similar to R functions such as \code{lm()}, \code{glm()},
#' and \code{lmer()}/\code{glmer()}. 
#' Currently only normal, Poisson, and binomial models are supported.
#'
#' @name LM
#' @author Ken Kellner
#'
#' @param formula An R formula, possibly with the parameters followed by 
#'  brackets containing indices. If there are no indices, the macro attempts
#'  to guess the correct indices from the context. Formulas can include
#'  random effects via lme4-style notation (e.g., \code{~ x + (1|group)})
#' @param family A description of the error distribution and link function to
#'  be used in the model. This can be a character string naming a family function, 
#'  a family function or the result of a call to a family function. See \code{?family}.
#'  Supported families are \code{gaussian} (default), \code{binomial}, and \code{poisson}.
#' @param coefPrefix Character. All model coefficient names will begin with this prefix.
#'  default is \code{"beta_"} (so 'x' becomes 'beta_x', etc.)
#' @param sdPrefix Character. All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priors List of prior specifications, generated using 
#'  \code{setPriors()}.
#' @param modelMatrixNames Logical indicating if parameters be named so they match the
#'  names one would get from R's \code{model.matrix}.
#' @param modelInfo Used internally by nimbleMacros; a list of model information such as constants and dimensions
#' @param .env Used internally by nimbleMacros; the environment where the model was created
#'
#' @return NIMBLE code for the linear model, GLM, or GLMM specified by the formula, including priors.
#'
#' @author Ken Kellner
#'
#' @examples
#' constants <- list(y = rnorm(10),
#'                   x = rnorm(10), 
#'                   x2 = factor(sample(letters[1:3], 10, replace=TRUE)))
#'  
#' code <- nimbleCode({
#'    LM(y ~ x + x2)
#' })
#' 
#' mod <- nimbleModel(code, constants = constants)
#' mod$getCode()
NULL

#' @export
LM <- list(process = function(code, modelInfo, .env){
  # This function doesn't (can't?) use the nimble macro creator
  # because it may not be an assignment
  # Break code into LHS and RHS
  if(isAssignment(code)){
    RHS <- getRHS(code)
    LHS <- getLHS(code)
  } else {
    LHS <- code[[2]][[2]] # LHS is the response variable in the formula
    code[[2]][[2]] <- NULL
    #code[[1]] <- NULL
    RHS <- code
  }
  
  # Get family
  family <- if(is.null(RHS$family)) quote(gaussian) else RHS$family
  family <- processFamily(family)
  link <- if(family$link == "identity") NULL else as.name(family$link)

  # Get modMatNames
  modMatNames <- RHS$modelMatrixNames
  if(is.null(modMatNames)) modMatNames <- FALSE

  # Create binomial sample size
  bin_agg <- FALSE
  if(family$family == "binomial"){
    if(!is.name(LHS) && LHS[[1]] == quote(cbind)){
      bin_agg <- TRUE
      modelInfo$constants$binSize <- eval(as.call(list(as.name("+"), LHS[[2]], LHS[[3]])), 
                                       envir=modelInfo$constants)
      LHS <- LHS[[2]]
    }
  }
  
  # Guess LHS bracket index if it's missing
  if(hasBracket(LHS)){
    idx <- extractIndices(LHS)[[1]]
  } else {
    # Check that LHS is in the constants, otherwise this won't work
    LHS_name <- safeDeparse(LHS)
    if(! LHS_name %in% names(modelInfo$constants)){
      stop(paste0("LM: If you don't provide dimensions for the data ", LHS_name, 
                  ", you must include ", LHS_name, " in the constants instead."), call.=FALSE)
    }
    idx <- substitute(1:LEN, list(LEN=as.numeric(length(modelInfo$constants[[LHS_name]]))))
    LHS <- substitute(LHS[IDX], list(LHS=LHS, IDX=idx))
  }
  
  par2 <- 1
  if(bin_agg){
    par2 <- substitute(binSize[IDX], list(IDX=idx))
  }

  # RHS formula
  form <- RHS[[2]]

  priors <- if(is.null(RHS$priors)) quote(setPriors()) else RHS$priors
  coefPrefix <- if(is.null(RHS$coefPrefix)) quote(beta_) else RHS$coefPrefix
  sdPrefix <- RHS$sdPrefix
  if(family$family == "gaussian"){
    if(is.null(sdPrefix)){
      par2 <- quote(sd_residual)
    } else {
      par2 <- str2lang(paste0(safeDeparse(sdPrefix),"sd_residual"))
    }
  }
  
  dataDec <- getDataDistCode(family$family, LHS, idx, par2)
  # FIXME: LHS par should not be fixed at mu
  LP <- substitute(mu_[IDX] <- LINPRED(FORM, link=LINK, coefPrefix=COEFPREFIX,
                                       sdPrefix=SDPREFIX, priors=PRIORS, 
                                       modelMatrixNames=MODMAT),
                   list(IDX=idx, FORM=form, COEFPREFIX=coefPrefix, SDPREFIX=sdPrefix,
                        PRIORS=priors, LINK=link, MODMAT=modMatNames))
  pars_added <- list(quote(mu_))
  out <- list(dataDec, LP)

  if (family$family == "gaussian"){
    priors <- eval(priors, envir=.env)
    sdPrior <- matchPrior(par2, "sd", priors = priors) 
    sigprior <- substitute(SDRES ~ SDPRIOR, list(SDPRIOR=sdPrior, SDRES=par2))
    pars_added <- c(pars_added, list(par2))
    out <- c(out, list(sigprior))
    new_inits <- list(1)
    names(new_inits) <- safeDeparse(par2)
    if(is.null(modelInfo$inits)) modelInfo$inits <- list()
    modelInfo$inits <- utils::modifyList(modelInfo$inits, new_inits) 
  }

  list(code=removeExtraBrackets(embedLinesInCurlyBrackets(out)),
       modelInfo = modelInfo)
})
class(LM) <- 'model_macro'

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
    stop("processFamily: 'family' not recognized: ", fam, call. = FALSE)
  }
  if(!fam$link %in% c("log", "identity","logit","probit","cloglog")){
    stop("processFamily: Unsupported link function: ", fam$link, call.=FALSE)
  }
  fam
}

# FIXME: This should be more general
getDataDistCode <- function(family, response, idx, dispParName){
  if(family == "gaussian"){
    out <- substitute(DAT ~ FORLOOP(DIST(mu_[IDX], sd=DISPPAR)),
                        list(DAT=response, DIST=quote(dnorm), IDX=idx,
                             DISPPAR=dispParName))
  } else if(family == "poisson"){
    out <- substitute(DAT ~ FORLOOP(DIST(mu_[IDX])),
                      list(DAT=response, DIST=quote(dpois), IDX=idx))
  } else if(family == "binomial"){
    # support other sizes
    out <- substitute(DAT ~ FORLOOP(DIST(mu_[IDX], size=N)),
                      list(DAT=response, DIST=quote(dbinom), N=dispParName, IDX=idx))
  } else {
    stop("getDataDistCode: Family not supported: ", family, call.=FALSE)
  }
  out
}
