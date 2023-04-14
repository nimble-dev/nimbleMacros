# Generate a code block with parameter priors for a given formula and 
# corresponding dataset
# Fixes some parameter values at 0 if necessary (i.e., reference levels for factors)
#' @importFrom stats model.matrix
makeFixedPriorsFromFormula <- function(formula, data, prior, prefix, modMatNames=FALSE){ 

  par_struct <- makeParameterStructure(formula, data)
  # Matching structure with the model matrix version of the names
  # Plugged in later if modMatNames = TRUE
  par_mm <- makeParameterStructureModMatNames(formula, data)

  par_names <- getParametersForLP(names(par_struct), prefix)

  getParametersForLP(colnames(model.matrix(formula, data)), prefix)

  all_priors <- lapply(1:length(par_struct), function(i){

    # Get all inds - they should be only 0 or 1 so <2 captures all
    inds <- as.matrix(which(par_struct[[i]] < 2, arr.ind=TRUE))
  
    if(nrow(inds) < 2){
      return(substitute(LHS ~ PRIOR, list(LHS=str2lang(par_names[i]), PRIOR=prior)))
    }

    lapply(1:nrow(inds), function(j){
      val <- par_struct[[i]][t(inds[j,])]
      bracket <- paste0("[",paste(inds[j,], collapse=","),"]")
      node <- str2lang(paste0(par_names[i], bracket))
      if(val){
        if(modMatNames){
          alt_par <- str2lang(paste0(prefix, par_mm[[i]][t(inds[j,])]))
          embedLinesInCurlyBrackets(
            list(substitute(LHS <- ALT, list(LHS=node, ALT=alt_par)),
                 substitute(ALT ~ PRIOR, list(ALT=alt_par, PRIOR=prior)))
          )
        } else {
          substitute(LHS ~ PRIOR, list(LHS=node, PRIOR=prior))
        }
      } else {
        substitute(LHS <- 0, list(LHS=node))
      }
    })
  })
  all_priors <- unlist(all_priors)
  # unroll interior brackets
  all_priors <- unlist(lapply(all_priors, function(x){
    if(x[[1]] == "{") x <- as.list(x)[2:length(x)]
    x
  }))
  
  embedLinesInCurlyBrackets(all_priors)
}

# Organize model.matrix() version of parameter names into an
# identical data structure to makeParameterStructure()
# So they can be matched if required
#' @importFrom stats model.matrix
makeParameterStructureModMatNames <- function(formula, data){
 
  # Generate placeholder structure containing all 0s
  empty_structure <- makeEmptyParameterStructure(formula, data)

  pars_full <- colnames(model.matrix(formula, data))
  pars_sep <- strsplit(pars_full, ":")
  pars_full <- gsub("(","", pars_full, fixed=TRUE)
  pars_full <- gsub(")", "", pars_full, fixed=TRUE)
  pars_full <- gsub(":", ".", pars_full, fixed=TRUE)
  
  # Replace elements of parameter structure for which we actually estimate
  # a parameter with 1
  lapply(empty_structure, function(x){
    for (i in 1:length(pars_sep)){
      ind <- t(pars_sep[[i]])
      if(length(dim(x)) == length(ind) & all(ind %in% unlist(dimnames(x)))){
        x[ind] <- pars_full[i]
      }
    }
    drop(x)
  })
}

#' Macro to build code for priors on a linear predictor from R formula
#'
#' Generates appropriate priors for a linear predictor derived from an 
#' R formula. As such it makes the most sense to use this macro together with
#' the linPred macro which takes similar arguments.
#'
#' @name priors
#' @author Ken Kellner
#'
#' @param formula An R formula The formula must be right-hand side only (e.g. ~x). 
#'  This must always be the first argument supplied to priors.
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param coefPrior BUGS code for prior on coefficients. Default is dnorm(0, sd=10).
#' @param sdPrior BUGS code for prior on dispersion parameters. Default is
#'  half-Cauchy T(dt(0, 0.1, 1), 0,).
#' @param modMatNames Logical, should parameters be named so they match the
#'  names you would get from R's model.matrix function?
#'
#' @examples
#' \donttest{
#' constants <- list(x = rnorm(10), 
#'                   x2 = factor(sample(letters[1:3], 10, replace=T)))
#'
#' # Just linear predictor
#' code <- nimbleCode({
#'   y[1:n] ~ linPred(~x + x2)
#'   beta. ~ priors(~x + x2, coefPrior = dnorm(0, sd = 5), modMatNames=TRUE)
#'  })
#' nimble:::codeProcessModelMacros(code, constants)$code
#' }
NULL

#' @importFrom lme4 nobars
#' @export
priors <- list(process=function(code, .constants, .env=env){
  form <- getRHS(code)[1:2][[2]]
  if(form[[1]] != quote(`~`)) form <- c(quote(`~`),form) 
  form <- as.formula(form)
  form <- removeBracketsFromFormula(form)
  
  coefPrefix <- getLHS(code)
  sdPrefix <- getRHS(code)$sdPrefix
  coefPrior <- getRHS(code)$coefPrior
  sdPrior <- getRHS(code)$sdPrior
  if(is.null(coefPrior)) coefPrior <- quote(dnorm(0, 10))
  if(is.null(sdPrior)) sdPrior <- quote(T(dt(0, 0.1, 1), 0,))

  rand_info <- processAllBars(form, sdPrior, coefPrefix, sdPrefix, .constants) 
    
  new_form <- form
  if(!is.null(rand_info$formula)){
    new_form <- addFormulaTerms(list(lme4::nobars(form), rand_info$formula))
    new_form <- as.formula(new_form)
  }

  dat <- makeDummyDataFrame(new_form, .constants)

  modMatNames <- getRHS(code)$modMatNames
  if(is.null(modMatNames)){
    modMatNames <- FALSE
  }

  fixed <- makeFixedPriorsFromFormula(lme4::nobars(form), dat, coefPrior, 
                               prefix=as.character(deparse(coefPrefix)),
                               modMatNames = modMatNames)
  out <- list(fixed)
  if(!is.null(rand_info$code)) out <- c(out, list(rand_info$code))
  out <- embedLinesInCurlyBrackets(out)
  out <- removeExtraBrackets(out)
  
  list(code=out, constants=.constants)
})
class(priors) <- "model_macro"
