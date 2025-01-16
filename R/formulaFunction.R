#' Function to handle offset() in LINPRED
#'
#' Translates offset() in an R formula passed to LINPRED into corresponding
#' NIMBLE code for an offset in the linear predictor.
#'
#' @name offsetFormulaFunction
#'
#' @param x A formulaComponentFunction object created from an offset() term
#'
#'
NULL

#' @export
offsetFormulaFunction <- list(process = 
  function(x, defaultBracket, coefPrefix, sdPrefix, modelInfo, env, ...){
 
  # Get the code inside offset()
  interior <- x$lang[[2]]

  # Extract any brackets
  brack <- extractAllBrackets(interior)
  if(is.null(brack)) brack <- defaultBracket
  if(length(brack) > 1) stop("Can't handle multiple brackets inside offset()", call.=FALSE)

  # Remove brackets from code inside offset
  interior <- removeSquareBrackets(interior)

  # Handle any additional functions inside offset (such as log)
  if(length(interior) > 1){ # If >1, there must be another function call
    # Get the name of the function
    func <- interior[[1]]
    # Get all names inside the call and create the name of the new constant
    all_names <- get_all_names(interior)
    const_name <- sapply(all_names, safeDeparse)
    const_name <- paste(const_name, collapse="_")
    # Try to evaluate the code
    new_const <- try(eval(interior, modelInfo$constants))
    if(inherits(new_const, "try-error")){
      stop("Problem evaluating function inside offset()", call.=FALSE)
    }
    # Add the new constant to the output
    add_const <- list(new_const)
    names(add_const) <- const_name
    if(is.null(x$constants)) x$constants <- list()
    x$constants <- modifyList(x$constants, add_const)
    # Update the name of the term to put in the linear predictor
    interior <- str2lang(const_name)
  }
  
  # Combine the constant name and bracket to create the linear predictor term
  code <- paste0(safeDeparse(interior), brack)  
  x$linPredCode <- code
  x$priorCode <- NULL   # no prior code for an offset
  x
})
class(offsetFormulaFunction) <- "formulaFunction"

# Extract all 'names' from a call
# So qnorm(log(x)) would yield qnorm, log, x
get_all_names <- function(code){
  out <- get_all_names_recursive(code)
  unlist(out)
}

get_all_names_recursive <- function(code){
  if(is.name(code)) return(code)
  if(is.call(code)) return(lapply(code, get_all_names))
  NULL
}

#' Function to handle scale() in LINPRED
#'
#' Translates scale() in an R formula passed to LINPRED into corresponding
#' NIMBLE code (and new constant) for a scaled covariate.
#'
#' @name scaleFormulaFunction
#'
#' @param x A formulaComponentFunction object created from a scale() term
#'
#'
NULL

#' @export
scaleFormulaFunction <- list(process = 
  function(x, defaultBracket, coefPrefix, sdPrefix, modelInfo, env, ...){

  # Identify which interaction terms involve scale
  trms <- splitInteractionTerms(x$lang)
  has_scale <- !sapply(trms, is.name)

  bracks <- extractAllBrackets(x$lang)

  for (i in 1:length(trms)){
    if(!has_scale[i]){
      trm_idx <- safeDeparse(trms[[i]])
      if(trm_idx %in% names(bracks)){
        brack <- bracks[[trm_idx]]
      } else {
        brack <- defaultBracket
      }
      trms[[i]] <- str2lang(paste0(safeDeparse(trms[[i]]), brack))
    } else {
      interior <- trms[[i]][[2]]
      if(!is.name(interior)) stop("Can't handle expression inside scale()", call.=FALSE)

      # Extract any brackets
      trm_idx <- safeDeparse(interior)
      if(trm_idx %in% names(bracks)){
        brack <- bracks[[trm_idx]]
      } else {
        brack <- defaultBracket
      }

      # Remove brackets from code
      interior <- removeSquareBrackets(interior)
  
      # New term
      new_term <- paste0(safeDeparse(interior), "_scaled")

      # Get constant
      const <- modelInfo$constants[[safeDeparse(interior)]]
      if(is.null(const)) stop("Covariate inside scale() is missing from constants", call.=FALSE)
      if(!is.numeric(const)) stop("Covariate inside scale() must be numeric", call.=FALSE)

      # Make sure constant retains dimensions after being scaled
      out <- as.numeric(scale(const))
      if(!is.null(dim(const))){
        out <- array(out, dim=dim(const))
      }

      add_const <- list(out)
      names(add_const) <- new_term 

      # Add the new constant to the object
      if(is.null(x$constants)) x$constants <- list()
      x$constants <- modifyList(x$constants, add_const)
      trms[[i]] <- str2lang(paste0(new_term, brack))
    }
  }

  # Updated interaction
  x$lang <- str2lang(paste(sapply(trms, safeDeparse), collapse=":"))

  # Update class
  class(x)[1] <- "formulaComponentFixed"
  
  x
})
class(scaleFormulaFunction) <- "formulaFunction"


#' Function to handle I() in LINPRED
#'
#' Translates I() in an R formula passed to LINPRED into corresponding
#' NIMBLE code (and new constant) for a scaled covariate.
#' Only allows for expressions involving one covariate (not functions of covariates).
#'
#' @name IFormulaFunction
#'
#' @param x A formulaComponentFunction object created from an I() term
#'
#'
NULL

#' @export
IFormulaFunction <- list(process = 
  function(x, defaultBracket, coefPrefix, sdPrefix, modelInfo, env, ...){

  # Identify which interaction terms involve scale
  trms <- splitInteractionTerms(x$lang)
  has_I <- !sapply(trms, is.name)

  bracks <- extractAllBrackets(x$lang)

  for (i in 1:length(trms)){
    if(!has_I[i]){
      trm_idx <- safeDeparse(trms[[i]])
      if(trm_idx %in% names(bracks)){
        brack <- bracks[[trm_idx]]
      } else {
        brack <- defaultBracket
      }
      trms[[i]] <- str2lang(paste0(safeDeparse(trms[[i]]), brack))
    } else {
      interior <- trms[[i]][[2]]

      # Extract any brackets
      trm_idx <- all.vars(interior)
      if(length(trm_idx) > 1) stop("Cannot handle more than one variable in I()", call.=FALSE)
      if(trm_idx %in% names(bracks)){
        brack <- bracks[[trm_idx]]
      } else {
        brack <- defaultBracket
      }
  
      # New term
      new_term <- safeDeparse(interior)
      new_term <- gsub(" ", "", new_term)
      new_term <- gsub("\\^|\\*|\\+|\\-|\\/", "_", new_term)

      # Get constant
      const <- modelInfo$constants[[trm_idx]]
      if(is.null(const)) stop("Covariate inside I() is missing from constants", call.=FALSE)
      if(!is.numeric(const)) stop("Covariate inside I() must be numeric", call.=FALSE)

      # Evaluate expression
      out <- eval(interior, env = modelInfo$constants)
      add_const <- list(out)
      names(add_const) <- new_term 

      # Add the new constant to the object
      if(is.null(x$constants)) x$constants <- list()
      x$constants <- modifyList(x$constants, add_const)
      trms[[i]] <- str2lang(paste0(new_term, brack))
    }
  }

  # Updated interaction
  x$lang <- str2lang(paste(sapply(trms, safeDeparse), collapse=":"))

  # Update class
  class(x)[1] <- "formulaComponentFixed"
  
  x
})
class(IFormulaFunction) <- "formulaFunction"
