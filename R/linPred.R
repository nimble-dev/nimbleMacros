# Get list of terms from formula, adding intercept if necessary
#' @importFrom stats terms
getTerms <- function(formula){
  trm <- attr(terms(formula), "term.labels")
  if(attr(terms(formula), "intercept")) trm <- c("Intercept", trm)
  trm
}

# Generates list of factor levels (=how R would name them) for each variable in formula
# E.g. variable x2 with levels a and b would return c("x2a", "x2b")
# Non-factors just return the variable name
getLevels <- function(formula, data){
  pars <- all.vars(formula)
  out <- lapply(pars, function(x){
    if(!is.factor(data[[x]])) return(x)
    paste0(x, levels(data[[x]]))
  })
  names(out) <- pars
  as.list(out)
}

# For each parameter in the model, create a placeholder array with correct dimensions
# Full of 0s
# Intercept has no dimensions
# Continuous parameters are scalars
# Factors are vectors
# Interactions involving a factor are arrays
# e.g. if factor x has 2 levels, and factor y has 2 levels
# The resulting interaction parameter will be a 2 x 2 matrix
# The dimensions are named matching to the parameter names
makeEmptyParameterStructure <- function(formula, data){
  trm <- getTerms(formula)
  levs <- getLevels(formula, data)
  out <- lapply(strsplit(trm, ":"), function(x){
    dm <- sapply(x, function(y) length(levs[[y]]))
    dnames <- lapply(x, function(y) levs[[y]])
    array(0, dim=as.numeric(dm), dimnames=dnames)
  })
  names(out) <- trm
  out
}

# Output is a list equal in length to number of parameters in model.
# For each parameter in the model, generates an object corresponding to
# its dimensions (intercept = none, continuous par = scalar, factor = vector,
# interaction = array). Within each object, elements that are to be estimated
# by the model have a 1, and everything else is 0.
# So for example, a model ~1 + x where x is a factor with two levels,
# the resulting element for x will be c(0, 1) - only the parameter for level 2 is
# estimated. However for ~x - 1, the result would be c(1,1).
#' @importFrom stats model.matrix
makeParameterStructure <- function(formula, data){
 
  # Generate placeholder structure containing all 0s
  empty_structure <- makeEmptyParameterStructure(formula, data)

  pars_full <- colnames(model.matrix(formula, data))
  pars_full <- strsplit(pars_full, ":")
  
  # Replace elements of parameter structure for which we actually estimate
  # a parameter with 1
  lapply(empty_structure, function(x){
    for (i in 1:length(pars_full)){
      ind <- t(pars_full[[i]])
      if(length(dim(x)) == length(ind) & all(ind %in% unlist(dimnames(x)))){
        x[ind] <- 1
      }
    }
    drop(x)
  })
}

# Remove brackets and everything inside them from formula
# E.g. ~x[1:n] + x2[1:k] --> ~x + x2
#' @importFrom stats as.formula
removeBracketsFromFormula <- function(formula){
  out <- removeSquareBrackets(formula)
  as.formula(out)
}

removeSquareBrackets <- function(code){
  if(is.name(code)) return(code)
  if(code[[1]] == "["){
    out <- code[[2]]
  } else {
    if(is.call(code)){
      out <- lapply(code, removeSquareBrackets)
    } else {
      out <- code
    }
  }
  if(!is.name(out) & !is.numeric(out)){
    out <- as.call(out)
  }
  out
}

# Extract entire bracket structure
# "formula" is actually a formula component, e.g. quote(x[1:n])
extractBracket <- function(formula){
  stopifnot(hasBracket(formula))
  #out <- regmatches(deparse(formula), regexpr("\\[.*?\\]", deparse(formula)))
  #extract out to the last bracket in case of nested brackets
  out <- regmatches(safeDeparse(formula), regexpr("\\[.*\\]", safeDeparse(formula)))
  names(out) <- as.character(formula[[2]])
  out
}

extractAllBrackets <- function(formula){
  if(hasBracket(formula, recursive=FALSE)){
    out <- extractBracket(formula)
  } else{
    if(is.call(formula)){
      out <- lapply(formula, extractAllBrackets)
    } else {
      out <- NULL
    }
  }
  out <- unlist(out)
  if(is.call(out) | is.numeric(out)) out <- list(out) # always return a list
  return(out)
}

# Extract brackets and everything inside them from each term in RHS of formula
# If there are no brackets, create them based on bracket present on LHS of formula
# Returned as named vector of character strings
getFormulaBrackets <- function(formula, LHSidx){
  vars <- all.vars(removeBracketsFromFormula(formula))
  inds <- extractAllBrackets(formula)
  if(is.null(inds)){
    idx <- paste0("[",paste(sapply(LHSidx, safeDeparse), collapse=", "),"]")
    inds <- replicate(idx, n=length(vars))
    names(inds) <- vars
  }
  removeDuplicateIndices(inds) # Fix this later?
}

# If parameter appears more than once in formula, make sure the indices
# match, then remove the duplicates
removeDuplicateIndices <- function(inds){
  for (i in names(inds)){
    subinds <- inds[names(inds)==i]
    stopifnot(all(subinds == subinds[[1]]))
    inds <- inds[unique(names(inds))]
  }
  inds
}

# Get variable type(s) associated with each parameter in the model
# Returns Intercept, continuous, or factor
getVariableType <- function(formula, data){
  pars <- getTerms(formula)
  out <- lapply(strsplit(pars, ":"), function(x){
    sapply(x, function(y){
      if(y == "Intercept") return(y)
      if(is.factor(data[[y]])) return("factor")
      return("continuous")
    })
  })
  names(out) <- pars
  out
}

# Check that brackets are available for all variables and return them
matchVarsToBrackets <- function(vars, brackets){
  if(!all(vars %in% names(brackets))){
    stop("Missing bracket index on RHS of formula", call.=FALSE)
  }
  brackets[vars]
}

# Generate indices when a parameter involves a factor
# For example for factor covariate x2 in formula, the result is: 
# [x2_NEW[1:n]]. The _NEW part is necessary for now since nimble doesn't
# properly handle factors automatically, so I need to make a new numeric version
# UPDATE: I've now hacked this into nimble macro branch
# A 2-way factor interaction would look something like [x2[1:n], x3[1:n]] etc.
factorComponent <- function(type, brackets){
  vars <- names(type)[type=="factor"]
  if(length(vars) == 0) return(NULL)
  bracks <- matchVarsToBrackets(vars, brackets)
  indices <- paste0(vars, bracks) # _NEW part should be removed later
  paste0("[", paste(indices, collapse=", "), "]")
}

# Generate the data part of the component of the linear predictor
# When the associated variable is continuous
# For example if x1 is continuous the result is: * x1[1:n]
continuousComponent <- function(type, brackets){
  vars <- names(type)[type=="continuous"]
  if(length(vars) == 0) return(NULL)
  bracks <- matchVarsToBrackets(vars, brackets)
  indices <- paste0(vars, bracks)
  paste(" *", paste(indices, collapse=" * "))
}

# Get parameter names by adding prefix
getParametersForLP <- function(components, prefix="beta_"){
  components <- gsub("(","", components, fixed=TRUE) # possibly not necessary
  components <- gsub(")", "", components, fixed=TRUE) # ditto
  components <- gsub(":", "_", components, fixed=TRUE)
  paste0(prefix, components)
  #paste0("beta[",1:length(components),"]")
}

makeLPFromFormula <- function(formula, data, LHSidx, prefix){
  formula_nobrack <- removeBracketsFromFormula(formula)
  #pars <- all.vars(formula_nobrack)
  # Get structure of each parameter
  par_struct <- makeParameterStructure(formula_nobrack, data)
  # Get complete name of each parameter (prefix + original name)
  par_names <- getParametersForLP(names(par_struct), prefix)
  # Extract brackets from formula or create them if missing
  brackets <- getFormulaBrackets(formula, LHSidx)  
  # Data type for each variable in the formula (intercept, continuous, factor)
  types <- getVariableType(formula_nobrack, data)

  # Build each part of the LP from the component parameters, combining parameter names,
  # factor indices (if applicable) and continuous covariate data (if applicable)
  lp <- lapply(1:length(par_names), function(i){
    paste0(par_names[i], factorComponent(types[[i]], brackets),
          continuousComponent(types[[i]], brackets))
  })
  names(lp) <- par_names

  # Collapse the list into a single linear predictor
  str2lang(paste(unlist(lp), collapse=" + "))
}

# Add numeric version of factors to constants, e.g. x2 becomes x2_NEW
# This should not always be necessary to do, but at the moment
# nimble doesn't handle factors smoothly
#addNumericFactorsToConstants <- function(constants){
#  for (i in 1:length(constants)){
#    if(is.factor(constants[[i]])){
#      constants[[paste0(names(constants)[i], "_NEW")]] <- as.numeric(constants[[i]])
#    }
#  }
#  constants
#}

# Make a dummy data frame with all the required variables in a formula
# The purpose of this is just to tell model.matrix what type each variable is
# (factor or continuous)
# This handles variables in the formula that don't actually show up in the constants,
# e.g. that are created in the model itself
makeDummyDataFrame <- function(formula, constants){
  vars <- all.vars(removeBracketsFromFormula(formula))
  out <- vector("list", length(vars))
  names(out) <- vars
  for (i in vars){
    # If variable is not in constants, we assume it is created in the model
    # and that it is continuous
    if(! i %in% names(constants)){
      out[[i]] <- 0
    } else if (is.null(constants[[i]])){
      stop("List element ", i, " in constants is NULL", call.=FALSE)
    } else if (is.factor(constants[[i]]) | is.numeric(constants[[i]])){
      out[[i]] <- constants[[i]][1]
    } else if (is.character(constants[[i]])){
      out[[i]] <- as.factor(constants[[i]])[1]
    }
  }
  as.data.frame(out)
}

# When centering on some grouping factor, determine which fixed effect terms
# to drop from the formula
# Returns a character vector of terms (e.g. c("1", "x"))
centeredFormulaDropTerms <- function(formula, centerVar){
  if(is.null(centerVar)) return(NULL)
  bars <- lme4::findbars(formula)
  rfacts <- lapply(bars, getRandomFactorName)
  bars_keep <- bars[sapply(rfacts, function(x) x == centerVar)]
  if(length(bars_keep) == 0) return(NULL)
  form_comps <- lapply(bars_keep, function(x) x[[2]])

  comb_form <- sapply(form_comps, function(x){
    x <- list(as.name("~"), x)
    x <- as.formula(as.call(x))
    trms <- stats::terms(x)
    has_int <- attr(trms, "intercept")
    out <- attr(trms, "term.labels")
    if(has_int) out <- c("1", out)
    out
  })
  drop(comb_form)
}

# Combine fixed and random terms into final formula
makeAdjustedFormula <- function(formula, rand_formula, centerVar=NULL){
  # If there are no random effects just return the original formula
  if(is.null(rand_formula)) return(formula)
  # Find fixed terms to remove if centered
  adj <- centeredFormulaDropTerms(formula, centerVar)
  # Find fixed terms
  fixed_form <- lme4::nobars(formula)
  trms <- stats::terms(fixed_form)
  has_int <- attr(trms, "intercept")
  fixed_trms <- attr(trms, "term.labels")
  if(has_int) fixed_trms <- c("1", fixed_trms)
  # Remove the appropriate terms
  fixed_trms <- fixed_trms[!fixed_trms %in% adj]
  # Combine remaining fixed terms and random terms into a formula
  fixed_terms <- paste(fixed_trms, collapse=" + ")
  if(fixed_terms == ""){
    out <- list(as.name("~"), rand_formula)
    out <- as.formula(as.call(out))
  } else {
    out <- list(as.name("~"), str2lang(fixed_terms))
    out <- as.formula(as.call(out))
    out <- addFormulaTerms(list(out, rand_formula))
  }
  # Make sure the intercept is explicitly dropped if needed
  if("1" %in% adj) out <- addFormulaTerms(list(out, quote(-1)))

  out
}


#' Macro to build code for linear predictor from R formula
#'
#' Converts an R formula into corresponding code for a linear predictor in BUGS.
#' Options are available to specify a link function and to also generate
#' code for priors corresponding to the parameters in the linear predictor.
#'
#' @name linPred
#' @author Ken Kellner
#' 
#' @param formula An R formula, possibly with the parameters followed by 
#'  brackets containing indices. If there are no indices, the macro attempts
#'  to guess the correct indices from the context. The formula must be 
#'  right-hand side only (e.g. ~x). This must always be the first argument supplied
#'  to linPred.
#' @param link A link function (available in BUGS) which will be applied to the 
#'  left-hand-side (the response) in the final linear predictor. Default is none.
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta_ (so x becomes beta_x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priorSettings Prior specifications, should be generated with setPrior()
#' @param centerVar Grouping covariate to 'center' on in parameterization. By
#'  default all random effects have mean 0 as with lme4.
#' 
#' @examples
#' code <- nimbleCode({
#'   mu[1:10] <- linPred(~x + x2)
#' })
#'
#' mod <- nimbleModel(code)
#' mod$getCode()
NULL

#' @importFrom lme4 nobars
#' @export
linPred <- nimble::model_macro_builder(
function(stoch, LHS, formula, link=NULL, coefPrefix=quote(beta_),
         sdPrefix=NULL, priorSettings=setPriors(), centerVar=NULL, modelInfo, .env){

    formula <- as.formula(formula)

    # Get index on LHS to use if none are found in RHS formula
    LHS_ind <- extractIndices(LHS)
    if(!is.null(link)){
      LHS <- as.call(list(link, LHS))
    }
    
    # FIXME: clunky
    modelInfo_temp <- modelInfo
    modelInfo_temp$indexCreator <- NULL # don't want to iterate the index creator here
    eval_priors <- eval(priorSettings, envir=.env)
    rand_info <- processAllBars(formula, eval_priors, coefPrefix, sdPrefix, modelInfo_temp, centerVar)
    modelInfo$constants <- rand_info$modelInfo$constants
    
    new_form <- makeAdjustedFormula(formula, rand_info$formula, centerVar)
    
    # Make a dummy data frame to inform model.matrix with variable types
    dat <- makeDummyDataFrame(new_form, modelInfo$constants)
    # Make linear predictor from formula and data
    RHS <- makeLPFromFormula(new_form, dat, LHS_ind, coefPrefix)
    # Add forLoop macro to result
    RHS <- as.call(list(quote(forLoop), RHS))
    # Combine LHS and RHS
    code <- substitute(LHS <- RHS, list(LHS = LHS, RHS = RHS))

    if(!is.null(priorSettings)){
      priorCode <- substitute(priors(FORMULA, coefPrefix=COEFPREFIX, sdPrefix=SDPREFIX, 
                                     priorSettings=PRIORSET, modMatNames=TRUE, centerVar=CENTER),
                              list(COEFPREFIX=coefPrefix, FORMULA=formula, SDPREFIX=sdPrefix,
                                   PRIORSET=priorSettings, CENTER=centerVar))
      code <- embedLinesInCurlyBrackets(list(code, priorCode))
    }

    # Return code and model info
    list(code=code, modelInfo=modelInfo)
  },
use3pieces=TRUE,
unpackArgs=TRUE
)

# PRIORS FOR LINPRED-----------------------------------------------------------

# Generate a code block with parameter priors for a given formula and 
# corresponding dataset
# Fixes some parameter values at 0 if necessary (i.e., reference levels for factors)
#' @importFrom stats model.matrix
makeFixedPriorsFromFormula <- function(formula, data, priors, prefix, modMatNames=FALSE){ 

  par_struct <- makeParameterStructure(formula, data)
  # Matching structure with the model matrix version of the names
  # Plugged in later if modMatNames = TRUE
  par_mm <- makeParameterStructureModMatNames(formula, data)

  par_names <- getParametersForLP(names(par_struct), prefix)

  data_types <- sapply(names(par_struct), function(x, data){
    if(x == "Intercept") return(NULL)
    if(is.factor(data[[x]])) return("factor")
    "continuous"
  }, data = data)

  par_types <- ifelse(names(par_struct) == "Intercept", "intercept", "coefficient")

  all_priors <- lapply(1:length(par_struct), function(i){

    # Get all inds - they should be only 0 or 1 so <2 captures all
    inds <- as.matrix(which(par_struct[[i]] < 2, arr.ind=TRUE))
  
    if(nrow(inds) < 2){
      node <- str2lang(par_names[i])
      # Search in order: parameter name exactly, without brackets, data type, parameter type
      prior <- matchPrior(node, data_types[[i]], par_types[i], priorSettings=priors)
      return(substitute(LHS ~ PRIOR, list(LHS=node, PRIOR=prior)))
    }

    lapply(1:nrow(inds), function(j){
      val <- par_struct[[i]][t(inds[j,])]
      bracket <- paste0("[",paste(inds[j,], collapse=","),"]")
      node <- str2lang(paste0(par_names[i], bracket))
      if(val){
        if(modMatNames){
          alt_par <- str2lang(paste0(prefix, par_mm[[i]][t(inds[j,])]))
          prior <- matchPrior(alt_par, data_types[[i]], par_types[i], priorSettings=priors)
          embedLinesInCurlyBrackets(
            list(substitute(LHS <- ALT, list(LHS=node, ALT=alt_par)),
                 substitute(ALT ~ PRIOR, list(ALT=alt_par, PRIOR=prior)))
          )
        } else {
          prior <- matchPrior(node, data_types[[i]], par_types[i], priorSettings=priors)
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
  
  list(code=embedLinesInCurlyBrackets(all_priors), parameters=par_names)
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
  pars_full <- gsub(" ", "_", pars_full, fixed=TRUE)
  
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
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta_ (so x becomes beta_x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priorSettings List of prior specifications, should be generated using 
#'  setPriors()
#' @param modMatNames Logical, should parameters be named so they match the
#'  names you would get from R's model.matrix function?
#' @param centerVar Grouping covariate to 'center' on in parameterization. By
#'  default all random effects have mean 0 as with lme4.
#'
#' @examples
#' code <- nimbleCode({
#'   priors(~x + x2)
#' })
#' 
#' mod <- nimbleModel(code)
#' mod$getCode()
NULL

#' @importFrom lme4 nobars
#' @export

priors <- nimble::model_macro_builder(
function(form, coefPrefix=quote(beta_), sdPrefix=NULL, priorSettings=setPriors(), 
         modMatNames=FALSE, centerVar=NULL, modelInfo, .env){

  if(form[[1]] != quote(`~`)) form <- c(quote(`~`),form) 
  form <- as.formula(form)
  form <- removeBracketsFromFormula(form)
  
  priorSettings <- eval(priorSettings, envir=.env) 

  rand_info <- processAllBars(form, priorSettings, coefPrefix, sdPrefix, modelInfo, centerVar) 
    
  new_form <- form
  if(!is.null(rand_info$formula)){
    new_form <- addFormulaTerms(list(lme4::nobars(form), rand_info$formula))
    new_form <- as.formula(new_form)
  }

  dat <- makeDummyDataFrame(new_form, modelInfo$constants)

  fixed <- makeFixedPriorsFromFormula(lme4::nobars(form), dat, priorSettings, 
                               prefix=as.character(safeDeparse(coefPrefix)),
                               modMatNames = modMatNames)
  out <- list(fixed$code)
  if(!is.null(rand_info$code)) out <- c(out, list(rand_info$code))
  out <- embedLinesInCurlyBrackets(out)
  out <- removeExtraBrackets(out)
  
  list(code=out, modelInfo=modelInfo)
},
use3pieces=FALSE,
unpackArgs=TRUE
)
