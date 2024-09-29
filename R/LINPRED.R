# Get list of terms from formula, adding intercept if necessary
#' @importFrom stats terms
getTerms <- function(formula){
  trm <- attr(terms(formula), "term.labels")
  if(attr(terms(formula), "intercept")) trm <- c("Intercept", trm)
  trm
}

# Expand concise interaction notation and handle some -terms
# e.g. ~x*x3 - 1 - x becomes ~0 + x3 + x:x3
# this avoids some pitfalls when adding in random effects
expand_formula <- function(x){
  if(x == ~1) return(~1)
  trm <- attr(terms(x), "term.labels")
  has_int <- attr(terms(x), "intercept")
  if(!has_int) trm <- c("0", trm)
  stats::reformulate(trm)
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

# Check if formula contains a function - these are not (yet?) supported
# E.g. y~x + scale(z) will error
checkNoFormulaFunctions <- function(form){
  form <- removeBracketsFromFormula(form)
  form <- reformulas::nobars(form)
  form <- safeDeparse(form) 
  has_parens <- grepl("(", form, fixed=TRUE)
  if(has_parens){
    stop("Functions in formulas, such as scale() or I(), are not supported", call.=FALSE)
  }
  invisible()
}

# Extract entire bracket structure
# "formula" is actually a formula component, e.g. quote(x[1:n])
extractBracket <- function(formula){
  if(!hasBracket(formula)) stop("Formula should have bracket")
  #extract out to the last bracket in case of nested brackets
  out <- regmatches(safeDeparse(formula), regexpr("\\[.*\\]", safeDeparse(formula)))
  names(out) <- as.character(formula[[2]])
  out
}

# Extract all brackets from a formula
# by calling extractBracket recursively for all components of formula
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
    if(!all(subinds == subinds[[1]])) stop("Indices should be identical")
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

# Create linear predictor from a formula, a list of data, the 
# range index for the LHS of the code line (e.g. the 1:3 in y[1:3] ~ x) and the 
# desired prefix for the created intercept/slope parameters in the linear predictor
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
# NO LONGER USED
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
  bars <- reformulas::findbars(formula)
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
# formula is just the fixed-effects part
# rand_formula is the random effects formula after converting it from
# lme4 structure to more standard structure
# for example (1|group) becomes just group
# Then this function combines them into a "final" formula and centers if needed
makeAdjustedFormula <- function(formula, rand_terms, centerVar=NULL){
  # If there are no random effects just return the original formula
  if(is.null(rand_terms)) return(formula)

  # Find fixed terms
  fixed_form <- reformulas::nobars(formula)
  trms <- stats::terms(fixed_form)
  has_int <- attr(trms, "intercept")
  fixed_terms <- attr(trms, "term.labels")
  int <- ifelse(has_int, "1", "0")
  fixed_terms <- c(int, fixed_terms)

  # Find fixed terms to remove if centered
  drop_terms <- centeredFormulaDropTerms(formula, centerVar)
  fixed_terms <- fixed_terms[!fixed_terms %in% drop_terms]

  # Make sure intercept is forced to 0 if it's not in list of terms
  needs_0 <- !any(c("0", "1") %in% fixed_terms)
  if(needs_0) fixed_terms <- c("0", fixed_terms)

  # Add random effects part of formula
  all_terms <- c(fixed_terms, rand_terms)

  # Convert back to formula
  stats::reformulate(all_terms)
}

processFormula <- function(formula, centerVar){

  # Make sure formula is a formula
  formula <- as.formula(formula)

  # Check there aren't any functions in the formulas, error if there are
  nimbleMacros:::checkNoFormulaFunctions(formula)    
  fixed_form <- reformulas::nobars(formula)
  trms <- stats::terms(fixed_form)
  has_int <- attr(trms, "intercept")
  fixed_terms <- attr(trms, "term.labels")
  int <- ifelse(has_int, "1", "0")
  fixed_terms <- c(int, fixed_terms)
  drop_terms <- nimbleMacros:::centeredFormulaDropTerms(formula, centerVar)
  fixed_terms <- fixed_terms[!fixed_terms %in% drop_terms]
  needs_0 <- !any(c("0", "1") %in% fixed_terms)
  if(needs_0) fixed_terms <- c("0", fixed_terms)

  bars <- reformulas::findbars(formula)

  rand_terms <- character(0)
  if(!is.null(bars)){
    rand_terms <- lapply(bars, barToTerms, keep_idx=TRUE)
    rand_terms <- unlist(rand_terms)
  }

  comb_terms <- c(fixed_terms, rand_terms)
  new_form <- stats::reformulate(comb_terms)

  new_terms <- attr(stats::terms(new_form), "term.labels")
  has_int <- attr(stats::terms(new_form), "intercept")
  int <- ifelse(has_int, "1", "0")
  new_terms <- c(int, new_terms)
  final_form <- stats::reformulate(new_terms)

  form_nobrack <- removeBracketsFromFormula(new_form)
  new_terms <- attr(stats::terms(form_nobrack), "term.labels")
  has_int <- attr(stats::terms(form_nobrack), "intercept")
  int <- ifelse(has_int, "1", "0")
  new_terms <- c(int, new_terms)
  terms_ref <- strsplit(new_terms, ":")
  names(terms_ref) <- new_terms

  list(formula = final_form, terms = new_terms, terms_ref = terms_ref)
}

fixTerms <- function(trms, formula_info){
  check_terms <- strsplit(trms, ":")
  reorder_terms <- sapply(check_terms, function(x, ref){
    ind <- which(sapply(ref, function(z) identical(sort(z), sort(x))))
    formula_info$terms[ind]
  }, ref=formula_info$terms_ref)
  reorder_terms
}

#' Macro to build code for linear predictor from R formula
#'
#' Converts an R formula into corresponding code for a linear predictor in NIMBLE model code.
#' Options are available to specify a link function and to also generate
#' code for priors corresponding to the parameters in the linear predictor.
#'
#' @name LINPRED
#' @author Ken Kellner
#' 
#' @param formula An R formula, possibly with the parameters followed by 
#'  brackets containing indices. If there are no indices, the macro attempts
#'  to guess the correct indices from the context. The formula must be 
#'  right-hand side only (e.g. ~x). This must always be the first argument supplied
#'  to LINPRED.
#' @param link A link function which will be applied to the 
#'  left-hand-side (the response) in the final linear predictor. Default is none.
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta_ (so x becomes beta_x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priorSpecs Prior specifications, should be generated with setPrior()
#' @param noncenter Logical; use noncentered parameterization?
#' @param centerVar Grouping covariate to 'center' on in parameterization. By
#'  default all random effects have mean 0 as with lme4.
#' 
#' @examples
#' code <- nimbleCode({
#'   mu[1:10] <- LINPRED(~x + x2)
#' })
#'
#' mod <- nimbleModel(code)
#' mod$getCode()
NULL

#' @export
LINPRED <- nimble::model_macro_builder(
function(stoch, LHS, formula, link=NULL, coefPrefix=quote(beta_),
         sdPrefix=NULL, priorSpecs=setPriors(), 
         noncenter = FALSE, centerVar=NULL, modelInfo, .env){

    form_info <- processFormula(formula, centerVar)

    # Get index range on LHS to use if the RHS formulas do not specify them
    LHS_ind <- extractIndices(LHS)
    # Get link function if it exists and wrap the LHS in the link
    # e.g. psi <- ... becomes logit(psi) <- ...
    if(!is.null(link)){
      LHS <- as.call(list(link, LHS))
    }
    
    # FIXME: clunky
    # Make a copy of the model info to use, but we don't want to iterate
    # the for loop index creator right now, so we get rid of it
    modelInfo_temp <- modelInfo
    modelInfo_temp$indexCreator <- NULL # don't want to iterate the index creator here
    # Get prior settings from setPriors() function
    eval_priors <- eval(priorSpecs, envir=.env)
    # Process bars e.g. (1|group) in the formula
    rand_info <- processAllBars(formula, eval_priors, coefPrefix, sdPrefix, modelInfo_temp, 
                                form_info, noncenter, centerVar)
    # Make adjustments to constants if needed (e.g. for nested random effects)
    modelInfo$constants <- rand_info$modelInfo$constants
    
    # Create new combined formula without random effects notation
    # e.g. ~x + (x||group) will become ~x + group + x:group
    #new_form <- makeAdjustedFormula(formula, rand_info$terms, centerVar)
    new_form <- form_info$formula
    
    # Make a dummy data frame to inform model.matrix with variable types
    dat <- makeDummyDataFrame(new_form, modelInfo$constants)
    # Make linear predictor from formula and data
    RHS <- makeLPFromFormula(new_form, dat, LHS_ind, coefPrefix)
    # Add FORLOOP macro to result
    RHS <- as.call(list(quote(nimbleMacros::FORLOOP), RHS))
    # Combine LHS and RHS
    code <- substitute(LHS <- RHS, list(LHS = LHS, RHS = RHS))

    # Add code for priors to output if needed
    if(!is.null(priorSpecs)){
      priorCode <- substitute(nimbleMacros::LINPRED_PRIORS(FORMULA, coefPrefix=COEFPREFIX, sdPrefix=SDPREFIX, 
                                     priorSpecs=PRIORSET, modMatNames=FALSE,
                                     noncenter = UNCENTER, centerVar=CENTERVAR),
                              list(COEFPREFIX=coefPrefix, FORMULA=formula, SDPREFIX=sdPrefix,
                                   PRIORSET=priorSpecs, UNCENTER = noncenter, CENTERVAR=centerVar))
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
  formula <- expand_formula(formula) 
  if(formula == ~0) return(list(code=NULL, parameters=character(0)))
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
      prior <- matchPrior(node, data_types[[i]], par_types[i], priorSpecs=priors)
      return(substitute(LHS ~ PRIOR, list(LHS=node, PRIOR=prior)))
    }

    lapply(1:nrow(inds), function(j){
      val <- par_struct[[i]][t(inds[j,])]
      bracket <- paste0("[",paste(inds[j,], collapse=","),"]")
      node <- str2lang(paste0(par_names[i], bracket))
      if(val){
        if(modMatNames){
          alt_par <- str2lang(paste0(prefix, par_mm[[i]][t(inds[j,])]))
          prior <- matchPrior(alt_par, data_types[[i]], par_types[i], priorSpecs=priors)
          embedLinesInCurlyBrackets(
            list(substitute(LHS <- ALT, list(LHS=node, ALT=alt_par)),
                 substitute(ALT ~ PRIOR, list(ALT=alt_par, PRIOR=prior)))
          )
        } else {
          prior <- matchPrior(node, data_types[[i]], par_types[i], priorSpecs=priors)
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
#' the LINPRED macro which takes similar arguments.
#'
#' @name LINPRED_PRIORS
#' @author Ken Kellner
#'
#' @param formula An R formula The formula must be right-hand side only (e.g. ~x). 
#'  This must always be the first argument supplied to LINPRED_PRIORS
#' @param coefPrefix All model coefficient names will begin with this prefix.
#'  default is beta_ (so x becomes beta_x, etc.)
#' @param sdPrefix All dispersion parameters will begin with this prefix.
#'  default is no prefix.
#' @param priorSpecs List of prior specifications, should be generated using 
#'  setPriors()
#' @param modMatNames Logical, should parameters be named so they match the
#'  names you would get from R's model.matrix function?
#' @param noncenter Logical, use noncentered parameterization?
#' @param centerVar Grouping covariate to 'center' on in parameterization. By
#'  default all random effects have mean 0 as with lme4.
#'
#' @examples
#' code <- nimbleCode({
#'   LINPRED_PRIORS(~x + x2)
#' })
#' 
#' mod <- nimbleModel(code)
#' mod$getCode()
NULL

#' @export

LINPRED_PRIORS <- nimble::model_macro_builder(
function(form, coefPrefix=quote(beta_), sdPrefix=NULL, priorSpecs=setPriors(), 
         modMatNames=FALSE, noncenter = FALSE, centerVar=NULL, modelInfo, .env){

  # Make sure formula is in correct format
  if(form[[1]] != quote(`~`)) form <- c(quote(`~`),form) 
  form <- as.formula(form)
  #form <- removeBracketsFromFormula(form)
  #checkNoFormulaFunctions(form)
  form_info <- processFormula(form, centerVar)
  
  priorSpecs <- eval(priorSpecs, envir=.env) 

  # Get random effects info (if any) from bar components for formula
  rand_info <- processAllBars(form, priorSpecs, coefPrefix, sdPrefix, modelInfo, 
                              form_info, noncenter, centerVar) 
  
  # Create new formula combining fixed effects and random effects
  # e.g. ~x + (x||group) becomes x + group + x:group
  #new_form <- makeAdjustedFormula(form_info$formula, rand_info$terms, centerVar)
  new_form <- removeBracketsFromFormula(form_info$formula)

  dat <- makeDummyDataFrame(new_form, modelInfo$constants)

  fixed <- makeFixedPriorsFromFormula(reformulas::nobars(form), 
                                      dat, priorSpecs,
                               prefix=as.character(safeDeparse(coefPrefix)),
                               modMatNames = modMatNames)
  out <- c(list(fixed$code), list(rand_info$code))
  out <- out[!sapply(out, is.null)]
  out <- embedLinesInCurlyBrackets(out)
  out <- removeExtraBrackets(out)
  
  list(code=out, modelInfo=modelInfo)
},
use3pieces=FALSE,
unpackArgs=TRUE
)
