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
#' @param modMatNames Logical, should parameters be named so they match the
#'  names you would get from R's model.matrix function?
#' @param noncenter Logical; use noncentered parameterization?
#' @param centerVar Grouping covariate to 'center' on in parameterization. By
#'  default all random effects have mean 0 as with lme4.
#' 
#' @examples
#' constants <- list(x = rnorm(3), x2 = factor(letters[1:3]))
#' code <- nimbleCode({
#'   mu[1:3] <- LINPRED(~x + x2)
#' })
#'
#' mod <- nimbleModel(code, constants=constants)
#' mod$getCode()
NULL

#' @export
LINPRED <- nimble::buildMacro(
function(stoch, LHS, formula, link=NULL, coefPrefix=quote(beta_),
         sdPrefix=NULL, priorSpecs=setPriors(), modMatNames = FALSE, 
         noncenter = FALSE, centerVar=NULL, modelInfo, .env){
    
    # Make sure formula is in correct format
    formula <- stats::as.formula(formula)
    
    # Get index range on LHS to use if the RHS formulas do not specify them
    LHS_ind <- extractBracket(LHS)

    # Split formula into components and process the components
    components <- buildLP(formula, defaultBracket = LHS_ind, 
                    coefPrefix = safeDeparse(coefPrefix),
                    modelInfo = modelInfo, centerVar = centerVar)
    
    # Update modelInfo
    modelInfo <- updateModelInfo(modelInfo, components)
    # Update initial values
    inits <- getInits(components)
    if(length(inits) > 0){
      if(is.null(modelInfo$inits)) modelInfo$inits <- list()
      modelInfo$inits <- utils::modifyList(modelInfo$inits, inits)
    }

    # Get combined linear predictor code
    # Get right side of linear predictor
    RHS <- getLP(components)
    # Get link function if it exists and wrap the LHS in the link
    # e.g. psi <- ... becomes logit(psi) <- ...
    if(!is.null(link)){
      LHS <- as.call(list(link, LHS))
    }
    code <- substitute(LHS <- nimbleMacros::FORLOOP(RHS), list(LHS = LHS, RHS = RHS))

    # Add code for priors to output if needed
    if(!is.null(priorSpecs)){
      priorCode <- substitute(nimbleMacros::LINPRED_PRIORS(FORMULA, coefPrefix=COEFPREFIX, sdPrefix=SDPREFIX, 
                                     priorSpecs=PRIORSET, modMatNames=MODMAT,
                                     noncenter = UNCENTER, centerVar=CENTERVAR),
                              list(COEFPREFIX=coefPrefix, FORMULA=formula, SDPREFIX=sdPrefix,
                                   PRIORSET=priorSpecs, MODMAT=modMatNames, 
                                   UNCENTER = noncenter, CENTERVAR=centerVar))
      code <- embedLinesInCurlyBrackets(list(code, priorCode))
    }

    # Return code and model info
    list(code=code, modelInfo=modelInfo)
  },
use3pieces=TRUE,
unpackArgs=TRUE
)


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
#' constants <- list(x = rnorm(3), x2 = factor(letters[1:3]))
#' code <- nimbleCode({
#'   LINPRED_PRIORS(~x + x2)
#' })
#' 
#' mod <- nimbleModel(code, constants = constants)
#' mod$getCode()
NULL

#' @export
LINPRED_PRIORS <- nimble::buildMacro(
function(formula, coefPrefix=quote(beta_), sdPrefix=NULL, priorSpecs=setPriors(), 
         modMatNames=FALSE, noncenter = FALSE, centerVar=NULL, modelInfo, .env){

  # Make sure formula is in correct format
  if(formula[[1]] != quote(`~`)) formula <- c(quote(`~`),formula) 
  formula <- stats::as.formula(formula)

  # Evaluate prior settings
  priorSpecs <- eval(priorSpecs, envir=.env) 

  # Split formula into components and process the components
  components <- buildLP(formula, defaultBracket = "[]", # default bracket not used below 
                    coefPrefix = safeDeparse(coefPrefix),
                    modelInfo = modelInfo, centerVar = centerVar)
    
  # Update constants in modelInfo
  modelInfo <- updateModelInfo(modelInfo, components)
  # Update initial values
  inits <- getInits(components)
  if(length(inits) > 0){
    if(is.null(modelInfo$inits)) modelInfo$inits <- list()
    modelInfo$inits <- utils::modifyList(modelInfo$inits, inits)
  }

  components <- buildPriors(components, coefPrefix=safeDeparse(coefPrefix), 
                            sdPrefix=sdPrefix, modelInfo = modelInfo, 
                            priorSpecs=priorSpecs,
                            modMatNames = modMatNames, noncenter=noncenter)

  # Get complete prior code
  code <- getPriors(components)
  
  list(code=code, modelInfo=modelInfo)
},
use3pieces=FALSE,
unpackArgs=TRUE
)


# This function starts with a formula plus options
# Splits the formula into separate components (terms)
# Then iteratively adds information to each component until eventually
# the linear predictor code for the components can be added
buildLP <- function(formula, defaultBracket, coefPrefix="beta_", modelInfo, centerVar=NULL){
  comps <- separateFormulaComponents(formula)
  # Functions will be handled here; for now an error
  is_function <- sapply(comps, function(x) inherits(x, "formulaComponentFunction"))
  if(any(is_function)) stop("Functions in formulas not yet supported", call.=FALSE)
  comps <- lapply(comps, addTermsAndBrackets, defaultBracket = defaultBracket, constants = modelInfo$constants)
  # Update constants in modelInfo with any new constants before moving on
  # constants may have been created by addTermsAndBrackets if there were nested random effects
  # It's important to do this now because later steps need to know that these
  # new constants are specifically factors
  modelInfo <- updateModelInfo(modelInfo, comps)
  comps <- lapply(comps, addParameterName, prefix = coefPrefix) 
  comps <- lapply(comps, addParameterStructure, constants = modelInfo$constants)
  comps <- lapply(comps, addLinPredCode)
  comps <- centeredFormulaDropTerms(comps, centerVar)
  comps <- lapply(comps, addInits)
  comps
}

# Get the linear predictor code from all components and combine it
getLP <- function(components){
  parcode <- unlist(sapply(components, function(x) x$linPredCode))
  parcode <- parcode[!sapply(parcode, is.null)]
  str2lang(paste(parcode, collapse = " + "))
}

# Get initial values from all components and return a named list
getInits <- function(components){
  inits <- lapply(components, function(x) x$inits)
  inits <- inits[!sapply(inits, is.null)]
  if(length(inits) == 0) return(NULL)
  inits <- Reduce(utils::modifyList, inits)
  inits
}

# Get new/updated constants from components
getConstants <- function(components){
  const <- lapply(components, function(x) x$constants)
  const <- const[!sapply(const, is.null)]
  if(length(const) == 0) return(list())
  const <- Reduce(utils::modifyList, const)
  const
} 

# Update modelInfo with any new constants in the components
updateModelInfo <- function(modelInfo, components){
  if(is.null(modelInfo$constants)){
    modelInfo$constants <- list()
  }
  modelInfo$constants <- utils::modifyList(modelInfo$constants, getConstants(components))
  modelInfo
}

# This function starts with the output from buildLP(), a list of components
# and iteratively adds more information needed to generate the prior code
# for each component
buildPriors <- function(components, coefPrefix="beta_", sdPrefix=NULL, modelInfo, priorSpecs,
                        modMatNames = FALSE, noncenter=FALSE){
  comps <- lapply(components, addDataType, constants = modelInfo$constants)
  fixed_pars <- getFixedParametersToEstimate(components, constants = modelInfo$constants)
  comps <- lapply(comps, fillParameterStructure, fixed_pars)
  comps <- lapply(comps, addPriorsCode, priorSpecs=priorSpecs, coefPrefix=coefPrefix, 
                  sdPrefix=sdPrefix, modMatNames=modMatNames, modelInfo = modelInfo, 
                  components=comps, noncenter=noncenter)
  comps <- lapply(comps, addInits, sdPrefix=sdPrefix)
  comps
}

# Get the prior code from all components and combine it
getPriors <- function(components){
  code <- lapply(components, function(x) x$priorCode)
  code <- code[!sapply(code, is.null)]
  code <- embedLinesInCurlyBrackets(code)
  removeExtraBrackets(code)
}

# Below is the code for all the individual operations on the components, in order

# separateFormulaComponents----------------------------------------------------

# First step is to separate formula terms into component objects
# There is a special component class for different types of linear predictor components
# such as intercepts, functions, fixed effects, random effects
separateFormulaComponents <- function(formula){
  int <- createInterceptComponent(formula)
  fixed <- createFixedComponents(formula)
  rand <- createRandomComponents(formula)
  c(int, fixed, rand)
}

# Basic structure of component object
# Each component represents one piece of the formula
# The rest of the functions in this file generally operate in a single component in isolation,
# updating its properties/slots
createBlankComponent <- function(addedClass){
  out <- list(lang = NULL,          # original component code as R language --> x[1:n]
              term = NULL,          # equivalent component term (as character) --> "x"
              bracket = NULL,       # brackets for term --> "[1:n]"
              parameter = NULL,     # parameter name --> "beta_x"
              type = NULL,          # covariate type, continous/factor
              structure = NULL,     # structure of parameter; continuous covariate would be scalar
              linPredCode = NULL,   # code to include in linear predictor
              extraCode = NULL,     # any extra code to include with linear predictor
              priorCode = NULL,     # code to include in priors
              inits = NULL,         # initial values as named list
              centered=FALSE,       # is the component centered?
              noncenter=FALSE,      # is the component using the noncentered parameterization?
              constants = NULL)     # new / modified constants added by the formula component
  class(out) <- c(addedClass, "formulaComponent")
  out
}

# Special component type for intercepts
createInterceptComponent <- function(formula){
  out <- createBlankComponent("formulaComponentIntercept")
  has_int <- attr(stats::terms(formula), "intercept") == 1
  out$lang <- ifelse(has_int, 1, 0)
  list(out)
}

# Fixed effect components
# This includes function-based components, which are identified by
# presence of (), e.g. scale(), I()
createFixedComponents <- function(formula){
  fixed <- reformulas::nobars(formula) # remove random terms first
  fixed_terms <- attr(stats::terms(fixed), "term.labels")
  if(length(fixed_terms) == 0) return(NULL)
  components <- lapply(fixed_terms, function(x){
    if(grepl("(", x, fixed=TRUE)){ # is this a function component?
      # Identify function components in formula and tag them
      out <- createBlankComponent("formulaComponentFunction")
    } else {
      out <- createBlankComponent("formulaComponentFixed")
    }
    out$lang <- str2lang(x)
    out
  })
  components
}

# Random effect components e.g. (1|x), identified using reformulas tools
createRandomComponents <- function(formula){
  rand <- reformulas::findbars(formula)
  if(length(rand) == 0) return(NULL)
  components <- lapply(rand, function(x){
    out <- createBlankComponent("formulaComponentRandom")
    out$lang <- x
    out
  })
  components
}


# addTermsAndBrackets----------------------------------------------------------
# Takes formula component, adds formula terms to term slot and splits out brackets into bracket slot
# Returns the updated component
# For example x[1:n] becomes "x" term and "[1:n]"
addTermsAndBrackets <- function(x, defaultBracket, ...) UseMethod("addTermsAndBrackets")

#' @exportS3Method NULL
addTermsAndBrackets.formulaComponentIntercept <- function(x, defaultBracket, ...){
  x$term <- as.character(x$lang)
  # intercept terms cannot have brackets
  x
}
.S3method("addTermsAndBrackets", "formulaComponentIntercept", addTermsAndBrackets.formulaComponentIntercept)

#' @exportS3Method NULL
addTermsAndBrackets.formulaComponentFunction <- function(x, defaultBracket, ...){
  x # don't do anything; function components have to handle this manually
}
.S3method("addTermsAndBrackets", "formulaComponentFunction", addTermsAndBrackets.formulaComponentFunction)

#' @exportS3Method NULL
addTermsAndBrackets.formulaComponentFixed <- function(x, defaultBracket, ...){
  # Strip bracket and convert language to character
  x$term <- safeDeparse(removeSquareBrackets(x$lang))

  # variables
  vars <- all.vars(str2lang(x$term))
  # default brackets
  brack <- rep(defaultBracket, length(vars))
  names(brack) <- vars

  # Add actual brackets if they exist
  actual_bracks <- extractAllBrackets(x$lang)
  for (i in 1:length(brack)){
    if(names(brack)[i] %in% names(actual_bracks)){
      idx <- which(names(brack)[i] == names(actual_bracks))
      brack[i] <- actual_bracks[idx]
    }
  }

  x$bracket <- brack
  x
}
.S3method("addTermsAndBrackets", "formulaComponentFixed", addTermsAndBrackets.formulaComponentFixed)

#' @exportS3Method NULL
addTermsAndBrackets.formulaComponentRandom <- function(x, defaultBracket, constants, ...){
  
  # Check for nested random effect and update bar expression/lang and constants
  RHS <- x$lang[[3]]
  is_nested <- is.call(RHS) && RHS[[1]] == ":"
  if(is_nested){
    processed <- processNestedRandomEffects(x$lang, constants)
    x$lang <- processed$barExp
    x$constants <- processed$constants
  }

  # Get the grouping (random) factor
  rfact <- safeDeparse(getRandomFactorName(x$lang, FALSE))

  # Expand LHS of bar [e.g. x in (x|group)] into full set of terms including intercept
  LHS <- stats::as.formula(as.call(list(as.name("~"), x$lang[[2]])))
  LHS <- removeBracketsFromFormula(LHS)
  trms <- attr(stats::terms(LHS), "term.labels")
  int <- as.logical(attr(stats::terms(LHS), "intercept"))

  # If only intercept on LHS return just factor
  # so (1|group) becomes group
  if(length(trms)==0 & int){
    all_terms <- rfact
  } else {
    # If there is more than 1 term on LHS, all of them are interacted with the grouping factor
    # so (1+x|group) becomes group + x:group
    
    # Create empty output
    all_terms <- character(0)

    # If intercept, add random factor to output
    if(int) all_terms <- c(all_terms, rfact)
  
    # Interact other terms with random factor and add to output list
    all_terms <- c(all_terms, sapply(trms, function(x){
      paste0(x,":",rfact)
    }))
  }
  x$term <- unname(all_terms)

  # Extract bracket
  # variables
  vars <- sapply(x$term, function(z) all.vars(str2lang(z)))
  vars <- unique(unlist(vars))
  # default brackets
  brack <- rep(defaultBracket, length(vars))
  names(brack) <- vars

  # Add actual brackets if they exist
  actual_bracks <- extractAllBrackets(x$lang)
  for (i in 1:length(brack)){
    if(names(brack)[i] %in% names(actual_bracks)){
      idx <- which(names(brack)[i] == names(actual_bracks))
      brack[i] <- actual_bracks[idx]
    }
  }
  x$bracket <- brack

  x
}
.S3method("addTermsAndBrackets", "formulaComponentRandom", addTermsAndBrackets.formulaComponentRandom)

#  Get factor name on RHS of bar
getRandomFactorName <- function(barExp, keep_idx = FALSE){
  out <- barExp[[3]]
  if(is.name(out)) return(out)
  if(out[[1]] == "["){
    if(keep_idx){
      return(out)
    } else{
      return(out[[2]])
    }
  }
  stop("Something went wrong")
}

# Does initial processing on bar expressions with nested random effects
# e.g. (1|group:group2)
# Does two things:
# 1. Converts a RHS term combo like group:group2 into a single term group_group2
#    which allows it to be used in formula and avoids : issues in BUGS
# 2. Actually creates new factor group_group2 in constants, which is a combination
#    of levels of group and group2 (following lme4)
processNestedRandomEffects <- function(barExp, constants){
  if(!isBar(barExp)) stop("Input is not bar expression")
  RHS <- barExp[[3]]
  is_nested <- is.call(RHS) && RHS[[1]] == ":"
  # If no nesting return inputs
  if(!is_nested) return(list(barExp=barExp, constants=constants))

  # Create new combined random factor term
  fac_names <- strsplit(safeDeparse(RHS), ":")[[1]]
  comb_name <- paste(fac_names, collapse="_")
  barExp[[3]] <- as.name(comb_name)

  # Generate new combined random factor and add it to constants
  # Only make new factor if it hasn't already been done
  out <- list()
  if(! comb_name %in% names(constants)){
    fac_dat <- constants[fac_names]
    fac_len <- sapply(fac_dat, length)
    if(!all(fac_len == fac_len[1])) stop("All factors should be same length")

    are_facs <- all(sapply(fac_dat, is.factor))
    are_chars <- all(sapply(fac_dat, is.character))
    if(!(are_facs | are_chars)) stop("At least one grouping cov is not factor")

    if(are_facs){
      new_fac <- apply(as.data.frame(fac_dat), 1, paste, collapse=":")
      new_fac <- factor(new_fac)
    } else if(are_chars){
      match_dim <- dim(fac_dat[[1]])
      if(!(all(sapply(lapply(fac_dat, dim), function(x) identical(x, match_dim))))){
        stop("All factor covs should have same dimensions")
      }
      if(is.null(match_dim)){
        new_fac <- apply(as.data.frame(fac_dat), 1, paste, collapse=":")
      } else {
        as_vecs <- as.data.frame(lapply(fac_dat, as.vector))
        new_fac <- apply(as_vecs, 1, paste, collapse=":")
        new_fac <- array(new_fac, dim=match_dim)
      }
    } else {
      stop("Invalid input", call.=FALSE)
    }
    out[[comb_name]] <- new_fac
  }

  list(barExp=barExp, constants=out)
}


# addParameterName-------------------------------------------------------------
# Using the component term(s), creates the final parameter names that
# will be used in the BUGS code
# For example, if term is 'x', parameter will be (by default) "beta_x"
# There will always be one parameter name per component except when dealing with
# a correlated random effect
# User can provide the prefix, the default is "beta_"

addParameterName <- function(x, prefix) UseMethod("addParameterName")

# Intercept parameter always called PREFIX_Intercept
#' @exportS3Method NULL
addParameterName.formulaComponentIntercept <- function(x, prefix){
  if(x$lang == 1){
    x$parameter <- paste0(prefix, "Intercept")
  } else {
    x$parameter <- NULL
  }
  x
}
.S3method("addParameterName", "formulaComponentIntercept", addParameterName.formulaComponentIntercept)

# Function parameter names must be handled manually
#' @exportS3Method NULL
addParameterName.formulaComponentFunction <- function(x, prefix){
  x
}
.S3method("addParameterName", "formulaComponentFunction", addParameterName.formulaComponentFunction)

# For fixed and random terms, the parameter name is just PREFIX_term
# where any : in the term name is replaced by _, for BUGS compatability
# So "x" becomes "beta_x", "x:y" becomes "beta_x_y"
#' @exportS3Method NULL
addParameterName.formulaComponent <- function(x, prefix){
  if(is.null(x$term)) x$parameter <- NULL
  param <- paste0(prefix, x$term)
  param <- gsub(":", "_", param, fixed=TRUE)
  x$parameter <- param
  x
}
.S3method("addParameterName", "formulaComponent", addParameterName.formulaComponent)


# addParameterStructure--------------------------------------------------------
# This adds the parameter structure (i.e., the dimensions of the parameter)
# to the component.

addParameterStructure <- function(x, constants) UseMethod("addParameterStructure")

# Intercept parameters have no dimensions by definition
#' @exportS3Method NULL
addParameterStructure.formulaComponentIntercept <- function(x, constants){
  x$structure <- numeric(0)
  x
}
.S3method("addParameterStructure", "formulaComponentIntercept", addParameterStructure.formulaComponentIntercept)

# Function components have to handle this internally (if necessary)
#' @exportS3Method NULL
addParameterStructure.formulaComponentFunction <- function(x, constants){
  x
}
.S3method("addParameterStructure", "formulaComponentFunction", addParameterStructure.formulaComponentFunction)

# For other components (fixed/random slopes) the structure depends on the
# structure of the corresponding covariate(s)
# For example a slope corresponding to a continuous covariate is just a scalar
# A slope corresponding to a factor covariate is a vector with length equal to the # of levels
# An interaction of continuous and factor is also a vector with length equal to # of levels
# An interaction of factor and factor is a matrix with # rows equal to levels of
# first factor, # of columns equal to levels of 2nd factor, and so on
#
# The row/column names of the structure are either the name of the covariate (for continuous)
# or the names of the factor levels (for factors / interactions with factors)
#' @exportS3Method NULL
addParameterStructure.formulaComponent <- function(x, constants){
  # Identify all covariates in each term by splitting on :
  # Note this will be a list with length = number of parameters
  # Usually length 1 except for correlated random effects
  trms_split <- strsplit(x$term, ":")

  # Iterate over each term
  out <- lapply(trms_split, function(trms){
    trms <- as.list(trms)
    # Iterate over the covariates within the term
    levs <- lapply(trms, function(trm){
      dat <- constants[[trm]]         # Pull the covariate from the constants
      is_factor <- is.factor(dat) | is.character(dat)
      if(!is_factor) return(trm) # For continous covs, just return the cov name
      if(is.character(dat)) dat <- factor(dat)
      levs <- gsub(" ", "_", levels(dat)) # remove spaces in level names
      levs                                # Otherwise return the factor levels
    })

    # For factors / factor interactions,
    # Combine the level names with the covariate name
    # So 'group' with levels 'a', 'b' becomes 'groupa' 'groupb'
    levs <- lapply(1:length(levs), function(i){
      if(length(levs[[i]]) > 1){
        levs[[i]] <- paste0(unlist(trms)[i], levs[[i]])
      }
      levs[[i]]
    })
    names(levs) <- unlist(trms)
    
    # Create array structure for parameter and name dimensions as needed
    # Array is empty (all NAs) by default, filled in a later step
    dims <- sapply(levs, length)
    array(NA, dim = as.numeric(dims), dimnames=levs)
  })
  names(out) <- x$term
  x$structure <- out
  x
}
.S3method("addParameterStructure", "formulaComponent", addParameterStructure.formulaComponent)

# addDataType------------------------------------------------------------------
# Identify the covariate data types (continuous or factor) for each component
# This is used only for identifying the most appropriate prior with matchPrior()

addDataType <- function(x, constants) UseMethod("addDataType")

# For intercept and function components do nothing
#' @exportS3Method NULL
addDataType.formulaComponentIntercept <- function(x, constants) x
.S3method("addDataType", "formulaComponentIntercept", addDataType.formulaComponentIntercept)
#' @exportS3Method NULL
addDataType.formulaComponentFunction <- function(x, constants) x
.S3method("addDataType", "formulaComponentFunction", addDataType.formulaComponentFunction)

# For fixed/random, identify the covariates included in the terms
# of the component, look them up in the constants,
# and identify their type (continuous or factor)
#' @exportS3Method NULL
addDataType.formulaComponent <- function(x, constants){
  trm_split <- strsplit(x$term, ":")
  types <- lapply(trm_split, function(trm){
    sapply(trm, function(z){
      dat <- constants[[z]]
      if(is.numeric(dat)){
        return("continuous")
      } else if(is.factor(dat) | is.character(dat)){
        return("factor")
      } else {
        return("continuous")
      }
    })
  })
  names(types) <- x$term
  x$type <- types
  x
}
.S3method("addDataType", "formulaComponent", addDataType.formulaComponent)


#addLinPredCode----------------------------------------------------------------
# Using the information generated previously, create the BUGS code
# to be included in the linear predictor for each formula component
# For example, a component x where x is a continous covariate
# would yield: beta_x * x[1:n]

addLinPredCode <- function(x) UseMethod("addLinPredCode")

# For intercept components, the linear predictor chunk is just the
# parameter name, e.g. beta_Intercept
#' @exportS3Method NULL
addLinPredCode.formulaComponentIntercept <- function(x){
  x$linPredCode <- x$parameter
  x
}
.S3method("addLinPredCode", "formulaComponentIntercept", addLinPredCode.formulaComponentIntercept)

# As usual, function components will have to handle this on their own
#' @exportS3Method NULL
addLinPredCode.formulaComponentFunction <- function(x){
  x
}
.S3method("addLinPredCode", "formulaComponentFunction", addLinPredCode.formulaComponentFunction)

# For fixed/random components, use the parameter name, structure,
# and brackets to construct the corresponding BUGS code
#' @exportS3Method NULL
addLinPredCode.formulaComponent <- function(x){
  
  # Iterate over each parameter in the component
  # Generally will only be 1 except for correlated random effects
  out <- lapply(1:length(x$parameter), function(i){
    param_code <- x$parameter[i] # Start with parameter name
    dims <- dim(x$structure[[i]])
    dmnames <- names(attr(x$structure[[i]], "dimnames"))
    # Check if the parameter is not a scalar (i.e., a vector, or matrix)
    # If so it must be a factor or an interaction with at least one factor
    if(any(dims > 1)){
      # Get the index(es) for the factor
      # For example suppose you have a factor 'group' with 3 levels, of length n
      # The parameter will be 'beta_group', a vector of length 3
      # The vector will be indexed by 'group'
      # So the complete linear pred term will be: beta_group[group[1:n]]
      # You can also have a two (or more) factor interaction, 
      # in which case you needed indices for each factor.
      # Looks something like: beta_group1_group2[group1[1:n], group2[1:n]]
      fac_terms <- dmnames[dims > 1]
      fac_terms <- paste0(fac_terms, x$bracket[fac_terms])
      fac_terms <- paste(fac_terms, collapse = ", ")
      # Combine the index code with the parameter name code
      param_code <- paste0(param_code, "[", fac_terms, "]")
    }
    # Otherwise we're dealing with a continous covariate (say, x)
    # So we start with the parameter name beta_x
    # Then multiply it by the covariate vector: beta_x * x[1:n]
    if(any(dims == 1)){
      # Get the covariate name(s)
      num_terms <- dmnames[dims == 1]
      # Add the bracket(s) and combine
      num_terms <- paste0(num_terms, x$bracket[num_terms])
      num_terms <- paste(num_terms, collapse = " * ")
      # Combine with parameter name
      param_code <- paste(param_code, "*", num_terms)
    }
    param_code
  })
  x$linPredCode <- out
  x
}
.S3method("addLinPredCode", "formulaComponent", addLinPredCode.formulaComponent)


# Formula processing tools-----------------------------------------------------

# Create the complete formula from the components after
# processing random effects etc.
# Optionally drop random effects and function components
getFormula <- function(components, dropRandomComponents=FALSE, dropFunctionComponents=TRUE){
  if(dropRandomComponents) components <- dropRandomComponents(components)
  if(dropFunctionComponents) components <- dropFunctionComponents(components)
  trms <- sapply(components, function(x) x$term)
  trms <- unlist(trms[!sapply(trms, is.null)])
  stats::reformulate(trms)
}

dropFunctionComponents <- function(components){
  is_function <- sapply(components, function(x) inherits(x, "formulaComponentFunction"))
  components[!is_function]
}

dropRandomComponents <- function(components){
  is_random <- sapply(components, function(x) inherits(x, "formulaComponentRandom"))
  components[!is_random]
}


# Handle centering variables---------------------------------------------------
# With random effects models we may want to 'center' on the random grouping factor
# For example consider the model ~1 + x + (1 + x | group), with
# random intercepts and slopes by group
# By default, we include a 'mean effect' for the intercept and slope x in the 
# linear predictor, and the random slope and intercept components
# come from distributions with mean 0. So the linear predictor would look like
# beta_Intercept + beta_x * x[1:n] + beta_group[group[1:n]] + beta_x_group[group[1:n]] * x[1:n]
# and priors:
# beta_group ~ dnorm(0, sd_group)
# beta_x_group ~ dnorm(0, sd_x_group)
# However you could also pull the 'means' into the random effect distribution
# instead of including them separately in the linear predictor:
# beta_group[group[1:n]] + beta_x_group[group[1:n]] * x[1:n]
# with priors:
# beta_group ~ dnorm(beta_intercept, sd_group)
# beta_x_group ~ dnorm(beta_x, sd_x_group)
# These models are equivalent. You can do this by setting centerVar = group
# This function handles this by working through the components and erasing
# the linear predictor chunks for the appropriate components, and adjusting
# the priors for the appropriate random effects components based on centerVar
centeredFormulaDropTerms <- function(components, centerVar){
  # If there is no centerVar, don't do anything
  if(is.null(centerVar)) return(components)

  # Get the random grouping/factor covariate for each random effect component
  # For example (1|group) --> "group"
  rfacts <- sapply(components, function(x){
    if(!inherits(x, "formulaComponentRandom")) return("")
    safeDeparse(getRandomFactorName(x$lang, FALSE))
  })
  # Determine which of these components match the specified centerVar
  has_center <- rfacts == centerVar
  # If none match, do nothing
  if(!any(has_center)) return(components)

  # Set centered slot to TRUE for these random effect components
  for (i in 1:length(components)){
    if(has_center[i]){
      components[[i]]$centered <- TRUE
    }
  }

  # Identify which fixed-effect components terms need to be dropped from the
  # linear predictor based on the given centerVar
  # TODO: this maybe should be a separate function?
  drop_terms <- sapply(components[has_center], function(x){     # Iterate through centered components
    # Pull out terms in LHS of bar expression, e.g. (x||group) --> "1", "x" 
    LHS <- stats::as.formula(as.call(list(as.name("~"), x$lang[[2]])))
    LHS <- removeBracketsFromFormula(LHS) # remove any brackets
    trms <- attr(stats::terms(LHS), "term.labels")
    has_int <- as.logical(attr(stats::terms(LHS), "intercept")) # handle implied intercept
    if(has_int) trms <- c("1", trms)
    trms
  })
  drop_terms <- drop(drop_terms)

  # Drop intercept from linear predictor code if needed
  if("1" %in% drop_terms){
    # Identify which component is the intercept (it usually should be the first one,
    # but do this just in case)
    int_idx <- which(sapply(components, function(x) inherits(x, "formulaComponentIntercept")))
    # Only do this if there is an intercept
    if(length(int_idx) > 0){
      components[[int_idx]]$centered <- TRUE  # If intercept is dropped, set centered slot to TRUE for this component
      components[[int_idx]]["linPredCode"] <- list(NULL) # Make the linpred code NULL
    }
  }

  # Drop other fixed terms
  # Identify which fixed-effects components match the terms identified above
  is_fixed <- sapply(components, function(x) inherits(x, c("formulaComponentFixed")))
  trms <- sapply(components, function(x) x$term)
  trms_in_drop <- trms %in% drop_terms  
  drop_components <- is_fixed & trms_in_drop

  # Iterate through these components, setting centered to TRUE
  # and dropping the linear predictor code slot
  for (i in 1:length(components)){
    if(drop_components[i]){
      components[[i]]$centered <- TRUE
      components[[i]]$linPredCode <- replicate(length(components[[i]]$linPredCode), NULL) 
    }
  }

  # Return updated components
  components
}


# fillParameterStructure-------------------------------------------------------
# For a given component, take the previously created parameter structure
# which will be a scalar for a continous covariate, vector for factor, or matrix/array for factor interaction
# and identify which elements of that structure actually need to be estimated
# based on the model matrix
# For example, a factor with 3 levels will yield a parameter structure that's
# a vector with 3 elements. However if you are also estimating an intercept,
# we will only need to estimate 2 parameters. So the first element of the vector
# will be fixed at 0.
# We need to know which elements of these parameter vectors/matrices/arrays are
# fixed at 0 so we can set priors appropriately

fillParameterStructure <- function(x, fixedPars) UseMethod("fillParameterStructure")

#' @exportS3Method NULL
fillParameterStructure.formulaComponent <- function(x, fixedPars){
  x
}
.S3method("fillParameterStructure", "formulaComponent", fillParameterStructure.formulaComponent)

# Random effects don't need to worry about this issue so at the moment 
# they don't fill in the structure and just return the input
#' @exportS3Method NULL
fillParameterStructure.formulaComponentRandom <- function(x, fixedPars){
  x
}
.S3method("fillParameterStructure", "formulaComponentRandom", fillParameterStructure.formulaComponentRandom)

# Given a list of the names of the fixed-effect parameters to estimate
# (obtained from model.matrix, see below)
# Insert these names into the correct slots of the parameter structure
#' @exportS3Method NULL
fillParameterStructure.formulaComponentFixed <- function(x, fixedPars){
  if(is.null(fixedPars)) return(x)

  # Iterate over all parameter structures in the component, should only be 1
  new_struct <- lapply(x$structure, function(struct){
    struct[] <- "0" # Make all entries 0 to begin with
    # Get dimnames
    dimref <- unlist(dimnames(struct))
    for (i in 1:length(fixedPars)){ # iterate over all parameter elements to estimate
      ind <- t(fixedPars[[i]]) # the row/column names (=indices) of the parameter element to estimate
      if(length(dim(struct)) == length(ind) & all(ind %in% dimref)){
        ind[] <- ind[order(match(ind, dimref))]   # reorder inds to match if needed
        struct[ind] <- paste(ind, collapse="_") # insert the name of the parameter element into the cell
      }
    }
    struct
  })
  x$structure <- new_struct
  x
}
.S3method("fillParameterStructure", "formulaComponentFixed", fillParameterStructure.formulaComponentFixed)

# Get the list of fixed effect parameters to estimate from model.matrix
# For example, ~ 1 + x + a:b will yield
# list("(Intercept)", "x", c("a", "b"))
getFixedParametersToEstimate <- function(components, constants){
  # Get complete fixed effects formula
  fixed_formula <- getFormula(components, dropRandomComponents=TRUE, 
                           dropFunctionComponents=TRUE)
  # Create dummy data frame for use with model matrix
  dummy_df <- makeDummyDataFrame(fixed_formula, constants)
  # Get colnames of model.matrix
  fixed_pars <- colnames(stats::model.matrix(fixed_formula, dummy_df))
  if(length(fixed_pars) < 1) return(NULL)
  fixed_pars <- gsub(" ", "_", fixed_pars) # get rid of spaces in names
  strsplit(fixed_pars, ":") # split any interaction terms
}

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


# addPriorsCode----------------------------------------------------------------
# Generate code for priors for a formula component and add it to the priorCode slot
# Uses prior specifications as provided by priorSpecs()

addPriorsCode <- function(x, priorSpecs, ...){
  UseMethod("addPriorsCode")
}

#' @exportS3Method NULL
addPriorsCode.formulaComponent <- function(x, priorSpecs, ...){
  x
}
.S3method("addPriorsCode", "formulaComponent", addPriorsCode.formulaComponent)

# Intercept is simple
# Just need to make sure if there is no intercept there is also no prior code for it
#' @exportS3Method NULL
addPriorsCode.formulaComponentIntercept <- function(x, priorSpecs, ...){
  # If there is no intercept, make sure there is no prior code for it
  if(x$lang == 0){
    x["priorCode"] <- list(NULL)
    return(x)
  }
 
  # Create prior code
  par <- str2lang(x$parameter)
  prior <- nimbleMacros::matchPrior(par, "intercept", 
                                    priorSpecs = priorSpecs)
  code <- substitute(PAR ~ PRIOR, list(PAR=par, PRIOR = prior))
  x$priorCode <- code
  x
}
.S3method("addPriorsCode", "formulaComponentIntercept", addPriorsCode.formulaComponentIntercept)

# For fixed effect components, we can also specify if we want to use
# parameter names that match the names model.matrix generates (modMatNames = TRUE)
# This adds extra lines of BUGS code to the priors
#' @exportS3Method NULL
addPriorsCode.formulaComponentFixed <- function(x, priorSpecs, coefPrefix, modMatNames = FALSE, ...){

  # How many parameters in the component? should just be 1 for fixed effects
  npar <- length(x$structure)
  # Iterate over parameters (again should just be 1)
  code <- lapply(1:npar, function(i){
    struct <- x$structure[[i]]      # parameter structure
    param <- x$parameter[i]         # parameter name
    type <- x$type[[i]][1]          # Covariate data type (for interactions, use the first one)
    # Get all possible indices in the parameter structure
    # For example for a vector of length 3 it would be 1,2,3
    # for a 2x2 matrix it would be 1,1; 1,2; 2,1; 2,2 etc.
    inds <- which(struct == struct, arr.ind=TRUE) 

    # Iterate over all indices
    lapply(1:nrow(inds), function(j){
      if(nrow(inds) > 1){
        # If the parameter is not a scalar, create a bracket with the index
        bracket <- paste0("[",paste(inds[j,], collapse=","),"]")
      } else {
        bracket <- ""
      }
      # Combine parameter name and bracket and turn into code
      node <- str2lang(paste0(param, bracket))
      # Determine if this particular parameter element needs to be an estimated
      val <- struct[t(inds[j,])]
      # If the slot in structure is "0", that means we don't need to estimate
      # this element of the parameter, so we set the prior to 0
      if(val == "0"){
        return(substitute(NODE <- 0, list(NODE = node)))
      }

      # Create the model matrix version of the parameter name in case we need it
      modmat_par <- str2lang(paste0(coefPrefix, val))
      # Check if the name is identical to the default name
      node_modpar_match <- identical(modmat_par, node)
      # If we're using the model matrix name and it's different, 
      # create appropriate code
      # specifically we need to add a line equating the default and model matrix names (NODE<-MODPAR)
      if(modMatNames & !node_modpar_match){
        prior <- nimbleMacros::matchPrior(modmat_par, type, "coefficient", 
                                          priorSpecs = priorSpecs)
        code_part <- substitute({
          NODE <- MODPAR
          MODPAR ~ PRIOR
        }, list(NODE = node, MODPAR=modmat_par, PRIOR=prior))
      } else {
        # Otherwise just use the original name and we can skip one line of BUGS
        # code equating to the model matrix and default names
        prior <- nimbleMacros::matchPrior(node, type, "coefficient", 
                                          priorSpecs = priorSpecs)
        code_part <- substitute(NODE ~ PRIOR, list(NODE = node, PRIOR=prior))
      }
      code_part
    })
  })

  code <- embedLinesInCurlyBrackets(code)
  code <- removeExtraBrackets(code)
  x$priorCode <- code
  x
}
.S3method("addPriorsCode", "formulaComponentFixed", addPriorsCode.formulaComponentFixed)

# Add prior code for random effects component
#' @exportS3Method NULL
addPriorsCode.formulaComponentRandom <- function(x, priorSpecs, sdPrefix = NULL, modelInfo, components, noncenter=FALSE, ...){
  # Create hyperpriors on random effect SDs
  # If this is an uncorrelated random effect there will just be 1,
  # if it's correlated there will be more than one SD
  sd_prefix <- ifelse(is.null(sdPrefix), "", safeDeparse(sdPrefix))
  trm <- gsub(":", "_", x$term)
  sd_names <- paste0(sd_prefix, "sd_", trm)
  is_uncorrelated <- length(sd_names) == 1

  # Handle random factor slopes (for uncorrelated random variable only)
  # TODO: Put this in separate function?
  par_dim <- dim(x$structure[[1]])
  has_random_factor <- sum(par_dim > 1) > 1
  if(is_uncorrelated & has_random_factor){
    rfact <- safeDeparse(getRandomFactorName(x$lang, FALSE)) 
    dim_short_names <- strsplit(x$term, ":")[[1]]
    rfact_ind <- which(dim_short_names == rfact)
    nms <- attributes(x$structure[[1]])$dimnames
    nms[rfact_ind] <- rfact 
    nms <- expand.grid(nms)
    nms <- apply(nms, 1, function(x) paste(as.character(x), collapse="_"))
    sd_names <- paste0(sd_prefix, "sd_", nms) 
  }

  code1 <- lapply(sd_names, function(z){
    prior <- nimbleMacros::matchPrior(str2lang(z), "sd", priorSpecs = priorSpecs)
    substitute(SD ~ PRIOR,
               list(SD = str2lang(z), PRIOR = prior))
  })

  # Create priors on random effects
  if(is_uncorrelated){
    # If just one SD, this must be uncorrelated, so run uncorrelatedRandomPrior 
    code2 <- uncorrelatedRandomPrior(x, priorSpecs, sd_names, modelInfo$constants, components, noncenter)
  } else {
    # If more than one SD, must be correlated random effect
    code2 <- correlatedRandomPrior(x, priorSpecs, sdPrefix, sd_names, modelInfo, components, noncenter) 
  }

  # Combine the two code pieces
  code <- c(code1, code2)
  code <- embedLinesInCurlyBrackets(code)
  code <- removeExtraBrackets(code)
  x$priorCode <- code
  x$noncenter <- noncenter
  x  
}
.S3method("addPriorsCode", "formulaComponentRandom", addPriorsCode.formulaComponentRandom)

# Create uncorrelated random prior
uncorrelatedRandomPrior <- function(x, priorSpecs, sd_name, constants, components, noncenter=FALSE){
  # Get random grouping factor name
  rfact <- safeDeparse(getRandomFactorName(x$lang, FALSE))

  # Look it up in the constants asnd make sure it's a factor, and get the levels
  fac <- constants[[rfact]]
  if(!is.factor(fac)) stop("Grouping cov is not a factor", call.=FALSE)
  nlev <- length(levels(constants[[rfact]]))
  par_name <- x$parameter

  # Get parameter structure
  par_dim <- dim(x$structure[[1]])
  trm_split <- strsplit(x$term, ":")[[1]] 
  trm_split <- trm_split[par_dim > 1]
  rfact_ind <- which(trm_split== rfact)

  # Check if structure implies an interaction between two factors,
  # which means a random slope for a factor covariate
  has_random_factor <- sum(par_dim > 1) > 1
  if(has_random_factor){
    # Handle random slopes for factors
    pd <- par_dim
    pd <- lapply(pd, function(x) 1:x)
    pd[rfact_ind] <- paste0("1:",nlev)
    pd <- expand.grid(pd)
    pd <- apply(pd, 1, function(x) paste(x, collapse=","))
    idx <- sapply(pd, function(x) paste0("[", x, "]"))
  } else {
    # Simpler case of random slope for continous covariate
    idx <- paste0("[1:",nlev,"]")
  }
  # Get LHS of random effect assignment (parameter name + index)
  r_lhs <- paste0(par_name, idx)

  # By default random effects have mean 0
  rand_mean <- 0

  # If this random effect is centered, we have to adjust the means
  if(x$centered){
    if(has_random_factor){
      stop("Centering variable not supported for factor random slopes", call.=FALSE)
    }
    # Identify terms on the LHS of the bar expression
    LHS <- stats::as.formula(as.call(list(as.name("~"), x$lang[[2]])))
    LHS <- removeBracketsFromFormula(LHS)
    trms <- attr(stats::terms(LHS), "term.labels")
    has_int <- as.logical(attr(stats::terms(LHS), "intercept"))
    if(has_int) trms <- c("1", trms)
    stopifnot(length(trms) == 1)
    # Search through fixed effect components for ones that match these terms
    component_terms <- lapply(components, function(z) z$term)
    trm_ind <- which(component_terms == trms)
    # Identify the parameter name for these terms, which become the means
    # for the centered random effect
    rand_mean <- str2lang(components[[trm_ind]]$parameter)
    # If the parameter doesn't exist, re-set the mean to 0
    if(is.null(rand_mean)) rand_mean <- 0
  }

  # Build final code
  # If using noncentered parameterization, need to add an extra step to code
  code <- sapply(1:length(r_lhs), function(i){
    if(noncenter){
      r_lhs_raw <- paste0(par_name, "_raw", idx)
      out <- substitute({
        LHS_RAW ~ nimbleMacros::FORLOOP(dnorm(0, sd=1))
        LHS <- nimbleMacros::FORLOOP(MEAN + SD * LHS_RAW)
          }, 
        list(LHS=str2lang(r_lhs[i]), LHS_RAW = str2lang(r_lhs_raw[i]),
            MEAN=rand_mean, SD=str2lang(sd_name[i])))
    } else {
      out <- substitute(LHS ~ nimbleMacros::FORLOOP(dnorm(MEAN, sd=SD)),
        list(LHS=str2lang(r_lhs[i]), MEAN=rand_mean, SD=str2lang(sd_name[i])))
    }
    out
  })

  code <- embedLinesInCurlyBrackets(code)
  removeExtraBrackets(code)
}

# Create correlated random prior
correlatedRandomPrior <- function(x, priorSpecs, sdPrefix, sd_name, modelInfo, components, noncenter=FALSE){

  if(noncenter) stop("Noncentered parameterization not supported for correlated random effects", call.=FALSE)

  # Get grouping covariate and make sure it's a factor
  rfact <- safeDeparse(getRandomFactorName(x$lang, FALSE))
  fac <- modelInfo$constants[[rfact]]
  if(!is.factor(fac)) stop("Grouping cov is not a factor", call.=FALSE)
  # Figure out the number of levels of the factor
  nlev <- length(levels(modelInfo$constants[[rfact]]))
  
  # How many parameters are there? Should be at least 2
  par_names <- x$parameter
  np <- length(par_names)
  if(np < 2) stop("Need at least 2 terms")

  # Get structure of the parameters
  par_dims <- lapply(x$structure, dim)
  n_group_covs <- sapply(par_dims, function(z) sum(z > 1))
  # Won't work with random effects for factor slopes
  if(any(n_group_covs > 1)){
    stop("Correlated random slopes for factors not yet supported.\nTry converting to dummy variables instead.", call.=FALSE)
  }

  # Get code for vector of standard deviations
  # Insert separate parameter SDs into it
  sd_vec <- paste0(sdPrefix, "re_sds_", rfact)
  sds <- lapply(1:length(sd_name), function(i){
    substitute(SDS[IDX] <- SDPAR, 
               list(SDS = str2lang(sd_vec), IDX=as.numeric(i), SDPAR=str2lang(sd_name[i])))
  })
  sds <- embedLinesInCurlyBrackets(sds)

  # BUGS code for Ustar and U
  Ustar_name <- as.name(paste0("Ustar_", rfact))
  U_name <- as.name(paste0("U_", rfact))
  # LKJ distribution shape parameter
  eta <- priorSpecs$eta
  if(is.null(eta)) eta <- 1.3
  u <- substitute({
    USTAR[1:NP, 1:NP] ~ dlkj_corr_cholesky(ETA, NP)
    U[1:NP, 1:NP] <- uppertri_mult_diag(USTAR[1:NP, 1:NP], SDS[1:NP])
    }, list(USTAR=Ustar_name, U=U_name, NP=as.numeric(np), SDS=str2lang(sd_vec), ETA=eta)
  )

  # Get means for each random effect, default to 0
  re_means <- rep(0, length(par_names))
  # If centered, get the correct parameter name (see uncorrelated version above)
  if(x$centered){
    LHS <- stats::as.formula(as.call(list(as.name("~"), x$lang[[2]])))
    LHS <- removeBracketsFromFormula(LHS)
    trms <- attr(stats::terms(LHS), "term.labels")
    has_int <- as.logical(attr(stats::terms(LHS), "intercept"))
    if(has_int) trms <- c("1", trms)
    stopifnot(length(trms) > 1)
    component_terms <- lapply(components, function(z) z$term)

    re_means <- lapply(trms, function(z){
      trm_ind <- which(component_terms == z)
      if(length(trm_ind) == 0) return(0)
      out <- str2lang(components[[trm_ind]]$parameter)
      if(is.null(out)) out <- 0
      out
    })
  }

  # Get code to insert means into mean vector
  re_means_vec = paste0(sdPrefix, "re_means_", rfact)
  re_mean_loop <- lapply(1:length(par_names), function(i){
      substitute(MNS[IDX] <- MNPAR, 
                list(MNS = str2lang(re_means_vec), IDX=as.numeric(i), MNPAR=re_means[[i]]))
  })

  # Get index for use in code below, need to use index generator
  if(!is.null(modelInfo$indexCreator)){
    idx <- as.name(modelInfo$indexCreator())
  } else{
    idx <- quote(i_) # if no indexCreator is available, use a placeholder
  }

  # Generate BUGS code for B (the matrix of correlated random effects)
  B_name <- str2lang(paste0(sdPrefix, "B_", rfact))
  B <- substitute(B[IDX, 1:NP] ~ dmnorm(REMEANS[1:NP], cholesky = U[1:NP, 1:NP], prec_param=0),
                  list(B=B_name, IDX=idx, NP=as.numeric(np), REMEANS=str2lang(re_means_vec), U=U_name))
  
  # Generate BUGS code to split parts of B out into separate vectors for each parameter
  B_split <- lapply(1:np, function(j){
    substitute(PAR[IDX] <- B[IDX, J], 
               list(PAR=str2lang(par_names[j]), IDX=idx, B=B_name, J=as.numeric(j)))
  })

  # Generate BUGS code combining B and B_split into a for loop
  B_loop <- embedLinesInCurlyBrackets(c(B, B_split))
  B_loop <- c(list(as.name("for"), idx, substitute(1:NLEV, list(NLEV=as.numeric(nlev)))),
              B_loop)
  B_loop <- as.call(B_loop)

  # Return BUGS code combining all parts
  code <- embedLinesInCurlyBrackets(list(sds, u, re_mean_loop, B_loop))
  removeExtraBrackets(code)
}

# Nimble function needed above

#' uppertri_mult_diag
#' 
#' nimbleFunction needed when fitting correlated random effects.
#' Generates upper triangular Cholesky factor of covariance matrix (U in code)
#' from upper tri Cholesky factor of correlation matrix (Ustar in code)
#' and vector of standard deviations. Taken from the NIMBLE manual, 
#' section 5.2.4.1.2 LKJ distribution for correlation matrices.
#' 
#' @param mat upper triangular Cholesky factor of correlation matrix (Ustar)
#' @param vec vector of standard deviations for individual random effects
#'
#' @name uppertri_mult_diag

#' @importFrom nimble nimMatrix
#' @export
uppertri_mult_diag <- nimbleFunction(
    run = function(mat = double(2), vec = double(1)) {
        returnType(double(2))
        p <- length(vec)
        out <- matrix(nrow = p, ncol = p, init = FALSE)
        for(i in 1:p)
            out[ , i] <- mat[ , i] * vec[i]
        return(out)
})

# addInits---------------------------------------------------------------------
# Add default initial values for parameters in each component
# slopes/intercepts get default initial value(s) of 0, SDs get 1

addInits <- function(x, ...){
  UseMethod("addInits")
}

# By default do nothing
#' @exportS3Method NULL
addInits.formulaComponent <- function(x, ...){
  x
}
.S3method("addInits", "formulaComponent", addInits.formulaComponent)

# Intercept initial value is just 0
#' @exportS3Method NULL
addInits.formulaComponentIntercept <- function(x, ...){
  inits <- list(0)
  names(inits) <- x$parameter
  if(is.null(x$inits)) x$inits <- list()
  x$inits <- utils::modifyList(x$inits, inits) # to avoid duplicate entries
  x
}
.S3method("addInits", "formulaComponentIntercept", addInits.formulaComponentIntercept)

# Fixed effects inits are the parameter structure filled with 0s
#' @exportS3Method NULL
addInits.formulaComponentFixed <- function(x, ...){
  stopifnot(length(x$structure) == 1)
  inits <- unname(x$structure[[1]])
  # convert character matrix to numeric full of 0s
  inits[] <- "0"
  inits <- 'dim<-'(as.numeric(inits), dim(inits))
  inits <- drop(inits)
  inits <- list(inits)
  names(inits) <- x$parameter
  if(is.null(x$inits)) x$inits <- list()
  x$inits <- utils::modifyList(x$inits, inits)
  x
}
.S3method("addInits", "formulaComponentFixed", addInits.formulaComponentFixed)

# Random effects inits are parameter structure filled with 0s
# Random effect SDs are initialized to 1
#' @exportS3Method NULL
addInits.formulaComponentRandom <- function(x, sdPrefix=NULL, ...){

  # Initial values for parameters
  inits_par <- lapply(1:length(x$structure), function(i){
    out <- unname(x$structure[[i]])
    out[] <- "0"
    out <- 'dim<-'(as.numeric(out), dim(out))
    out <- drop(out)
    out
  })

  # Names of initial values
  par_names <- x$parameter
  # If this component has noncentered parameterization, modify parameter name
  if(x$noncenter) par_names <- paste0(x$parameter, "_raw")
  names(inits_par) <- par_names
  inits_par

  # Get SD names
  sd_prefix <- ifelse(is.null(sdPrefix), "", safeDeparse(sdPrefix))
  trm <- gsub(":", "_", x$term)
  sd_names <- paste0(sd_prefix, "sd_", trm)
  is_uncorrelated <- length(sd_names) == 1

  # Handle random factor slopes (for uncorrelated random variable only)
  par_dim <- dim(x$structure[[1]])
  has_random_factor <- sum(par_dim > 1) > 1
  if(is_uncorrelated & has_random_factor){
    rfact <- safeDeparse(getRandomFactorName(x$lang, FALSE)) 
    dim_short_names <- strsplit(x$term, ":")[[1]]
    rfact_ind <- which(dim_short_names == rfact)

    nms <- attributes(x$structure[[1]])$dimnames
    nms[rfact_ind] <- rfact 
    nms <- expand.grid(nms)
    nms <- apply(nms, 1, function(x) paste(as.character(x), collapse="_"))
    sd_names <- paste0(sd_prefix, "sd_", nms) 
  }
 
  # Create initial values for SDs
  inits_sd <- lapply(1:length(sd_names), function(i){
    1
  })
  names(inits_sd) <- sd_names
 
  # Combine initial values and add to component slot
  inits <- c(inits_par, inits_sd)
  if(is.null(x$inits)) x$inits <- list()
  x$inits <- utils::modifyList(x$inits, inits)
  x
}
.S3method("addInits", "formulaComponentRandom", addInits.formulaComponentRandom)
