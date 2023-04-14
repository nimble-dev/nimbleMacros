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
# This should probably be replaced with something that works on
# the code directly instead of using regular expressions
#' @importFrom stats as.formula
removeBracketsFromFormula <- function(formula){
  out <- gsub("\\[.*?\\]", "", deparse(formula))
  as.formula(gsub("\\[|\\]", "", out))
}

# Extract entire bracket structure
# "formula" is actually a formula component, e.g. quote(x[1:n])
extractBracket <- function(formula){
  stopifnot(hasBracket(formula))
  #out <- regmatches(deparse(formula), regexpr("\\[.*?\\]", deparse(formula)))
  #extract out to the last bracket in case of nested brackets
  out <- regmatches(deparse(formula), regexpr("\\[.*\\]", deparse(formula)))
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
    idx <- paste0("[",paste(sapply(LHSidx, deparse), collapse=", "),"]")
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
getParametersForLP <- function(components, prefix="beta."){
  components <- gsub("(","", components, fixed=TRUE) # possibly not necessary
  components <- gsub(")", "", components, fixed=TRUE) # ditto
  components <- gsub(":", ".", components, fixed=TRUE)
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
    } else if (is.factor(constants[[i]]) | is.numeric(constants[[i]])){
      out[[i]] <- constants[[i]][1]
    }
  }
  as.data.frame(out)
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
#' constants <- list(x = rnorm(10), 
#'                   x2 = factor(sample(letters[1:3], 10, replace=T)))
#'
#' # Just linear predictor
#' code <- nimbleCode({
#'   y[1:n] ~ linPred(~x + x2)
#' })
#' nimble:::codeProcessModelMacros(code, constants)$code
#' 
#' # Also generate matching priors
#' code <- nimbleCode({
#'   y[1:n] ~ linPred(~x + x2, coefPrior=dnorm(0, sd=10))
#' })
#' nimble:::codeProcessModelMacros(code, constants)$code
#' }
NULL

#' @importFrom lme4 nobars
#' @export
linPred <- list(
  process = function(code, .constants, .env){
    RHS <- getRHS(code)
    
    # Get value for prefix argument
    coefPrefix <- RHS$coefPrefix
    if(is.null(coefPrefix)){
      coefPrefix <- quote(beta.)
    } else {
      RHS$coefPrefix <- NULL 
    }
    sdPrefix <- RHS$sdPrefix
    if(!is.null(sdPrefix)){
      RHS$sdPrefix <- NULL 
    }

    # Get value for link argument
    link <- RHS$link
    if(!is.null(link)){
      RHS$link <- NULL 
    }
    coefPrior <- RHS$coefPrior
    if(!is.null(coefPrior)){
      RHS$coefPrior <- NULL
    }
    sdPrior <- RHS$sdPrior
    if(!is.null(sdPrior)){
      RHS$sdPrior <- NULL
    }

    # Get formula
    RHS <- RHS[[2]]
    if(RHS[[1]] != quote(`~`)) RHS <- c(quote(`~`),RHS) # 
    form <- as.formula(RHS)
    # Get index on LHS to use if none are found in RHS formula
    LHS_ind <- extractIndices(getLHS(code))
    if(!is.null(link)){
      LHS(code) <- as.call(list(link, getLHS(code)))
    }
    
    rand_info <- processAllBars(form, sdPrior, coefPrefix, sdPrefix, .constants)
    .constants <- rand_info$constants
    
    new_form <- form
    if(!is.null(rand_info)){
      new_form <- addFormulaTerms(list(lme4::nobars(form), rand_info$formula))
    }
    
    # Convert factors to numeric in constants (may not always be necessary)
    #newConstants <- addNumericFactorsToConstants(.constants)
    # Make a dummy data frame to inform model.matrix with variable types
    dat <- makeDummyDataFrame(new_form, .constants)
    # Make linear predictor from formula and data
    out <- makeLPFromFormula(new_form, dat, LHS_ind, coefPrefix)
    # Add forLoop macro to result
    out <- as.call(list(quote(forLoop), out))
    # Replace RHS with result
    RHS(code) <- out

    if(!is.null(coefPrior) | !is.null(sdPrior)){

      if(is.null(coefPrior)) coefPrior <- quote(dnorm(0, sd=10))
      if(is.null(sdPrior)) sdPrior <- quote(T(dt(0, 0.1, 1), 0,))

      priorCode <- substitute(PREFIX ~ priors(FORMULA, coefPrior=COEFPRIOR, sdPrefix=SDPREFIX, 
                                              sdPrior=SDPRIOR, modMatNames=TRUE),
                              list(PREFIX=coefPrefix, FORMULA=form, SDPREFIX=sdPrefix,
                                   COEFPRIOR=coefPrior, SDPRIOR=sdPrior))
      code <- embedLinesInCurlyBrackets(list(code, priorCode))
    }

    # Return code and new constants
    list(code=code, constants=.constants)
  }
)
class(linPred) <- "model_macro"
