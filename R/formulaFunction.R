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
