# Is chunk of code assigning a value or distribution to LHS
# declaration
isAssignment <- function(code){
  as.character(code[[1]]) %in% c("~", "<-")
}

# If "assignment" get LHS
getLHS <- function(code){
  if(!isAssignment(code)) stop("Code should have assignment")
  code[[2]]
}

# If "assignment" get RHS
getRHS <- function(code){
  if(!isAssignment(code)) stop("Code should have assignment")
  code[[3]]
}

# Replace LHS of assignment code chunk
`LHS<-` <- function(code, value){
  if(!isAssignment(code)) stop("Code should have assignment")
  if(!(is.call(value)|is.name(value))) stop("Value should be call or name")
  code[[2]] <- value
  code
}

# Replace RHS of assignment code chunk
`RHS<-` <- function(code, value){
  if(!isAssignment(code)) stop("Code should have assignment")
  if(!(is.call(value)|is.name(value)|is.numeric(value))){
    stop("Value should be call, name, or numeric")
  }
  code[[3]] <- value
  code
}

# Function copied from the nimble package
embedLinesInCurlyBrackets <- function(lines) {
  as.call(c(list(quote(`{`)), lines))
}

# not the same as nimble's version
safeDeparse <- function(inp) {
  out <- deparse(inp)
  paste(sapply(out, trimws), collapse=" ")
}

# Remove extra brackets in BUGS code
removeExtraBrackets <- function(code){
  as.call(removeExtraBracketsInternal(code))
}

removeExtraBracketsInternal <- function(code){
  unlist(lapply(code, function(x){
    if(length(x) == 1) return(x)                       
    if(x[[1]] == "{") x <- as.list(x)[2:length(x)]
    if(is.list(x)){
      x <- removeExtraBracketsInternal(x)
    } else if(x[[1]] == "for"){
      x[[4]] <- removeExtraBrackets(x[[4]])
    }
    x
  }))
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

# Get for loop index range from a chunk of code
forInfo <- function(x){
  if(is.symbol(x)) return(NULL)
  if(x[[1]] != "for") return(NULL)
  as.list(x[[3]])
}

# Attempt to combine for loops that share the same index range
collapseLoopsInternal <- function(code){
  if(is.symbol(code)) return(code)
  # Iterate over call components (except last one) 
  for (i in 1:(length(code)-1)){
    # Skip if just a symbol
    if(is.symbol(code[[i]])) next
    # Skip if not a for loop
    if(code[[i]][[1]] != "for") next
    # Get index range info from for loop
    i_info <- forInfo(code[[i]])
    # Iterate over all subsequent call components after this one
    # looking for matching index ranges
    for (j in (i+1):length(code)){
      # Skip if just a symbol
      if(is.symbol(code[[j]])) next
      # Skip if not a for loop
      if(code[[j]][[1]] != "for") next
      # Get index range info from the new for loop
      j_info <- forInfo(code[[j]])
      # Check if the two index ranges match
      if(identical(i_info, j_info)){
        # Save existing for loop code for "parent" loop into new variable
        newloop <- code[[i]]
        # Get loop index for this loop
        idx_i <- newloop[[2]]
        # Separate code inside loop
        internal <- newloop[[4]]
        # Drop the containing bracket from it
        internal[[1]] <- NULL
        # Get the code inside the "child" loop which will be added
        # to the parent loop
        # Note: could be a list if there is more than one line
        add_code <- code[[j]][[4]]
        # Remove bracket
        add_code[[1]] <- NULL
        # Get loop index for the "child" loop
        idx_j <- code[[j]][[2]]
        # Replace the existing index with the index from the parent loop
        add_code <- lapply(add_code, recursiveReplaceIndex, idx_j, idx_i)
        # Combine the parent and child loop code and insert it back into the for loop
        newloop[[4]] <- embedLinesInCurlyBrackets(c(internal, add_code))
        # Insert the for loop into the full code
        code[[i]] <- newloop
        # Mark the now-duplicated "child" code for removal later
        code[[j]] <- "_REMOVE_" 
      }
    }
  }

  # Return all code parts except the stuff to be removed
  code[!sapply(code, function(x) x == "_REMOVE_")]
}

# Recursively combine loops that share common index ranges
collapseLoops <- function(code){
  # Run the internal loop collapsing code once
  code <- collapseLoopsInternal(code)
  # Iterate over the result, looking for internal for loops and collapsing those
  if(is.call(code)){
    out <- lapply(code, function(x){
      if(is.symbol(x)) return(x)
      if(x[[1]] == "for"){
        x[[4]] <- collapseLoops(x[[4]])
      }
      x
    })
    out <- as.call(out)
  } else {
    out <- code
  }
  out
}

# Replace complex indices (i_1, i_2, etc.) with a smaller number of
# simplier indices (i, j, etc.) if possible
simplifyIndices <- function(code, new_indices){
  out <- lapply(code, function(x){
    if(is.name(x) | is.symbol(x)) return(x)
    if(x[[1]] == "for"){    
      unique_idx <- unique(extractAllIndices(x))
      # Ignore any remaining index *ranges*
      unique_idx <- unique_idx[sapply(unique_idx, is.name)]
      # Ignore blank indices as in x[i,]
      unique_idx <- unique_idx[sapply(unique_idx, function(x) x!="")]
      if(length(unique_idx) > length(new_indices)){
        stop("Not enough new indices provided", call.=FALSE)
      }
      for (i in 1:length(unique_idx)){
        x <- replaceForLoopIndex(x, unique_idx[[i]], new_indices[[i]])
      }
    }
    x
  })
  as.call(out)
}

# Replace the index in a loop recursively
replaceForLoopIndex <- function(code, idx, newidx){
  if(is.name(code) | is.symbol(code)) return(code)
  if(code[[1]] == "for"){
    if(code[[2]] == idx) code[[2]] <- newidx
    code[[4]] <- recursiveReplaceIndex(code[[4]], idx, newidx)
    if(is.call(code[[4]])){
      code[[4]] <- as.call(lapply(code[[4]], function(x) 
                                  replaceForLoopIndex(x, idx, newidx)))
    }
  }
  code
}

#' Simplify for loop structure in NIMBLE model code
#'
#' Takes the code for a NIMBLE model and attempts to combine for loops
#' that share the same index range, in order to simplify the code
#' structure. Optionally, can also replace existing for loop indices
#' with a (potentially smaller, simpler) set of new indices.
#' This function is particularly useful for simplifying code generated by
#' macros, which often creates many for loops with the same indices and 
#' uses complex indices like 'i_1', 'i_2', etc. which could be simplified to
#' 'i', 'j', etc.
#'
#' @author Ken Kellner
#'
#' @param code NIMBLE code for a model, such as from the output of model$getCode()
#' @param new_indices A list of new for loop indices that will replace the existing
#'  indices. The new indices must be quoted values (i.e., "names"/symbols).
#'  If NULL, letters starting with 'i' will be used. If FALSE, no indices will
#'  be replaced.
#'
#' @export
simplifyForLoops <- function(code, new_indices = NULL){
  out <- collapseLoops(code)
  if(is.null(new_indices)){
    new_indices <- lapply(letters[9:26], str2lang)
  } else if(is.logical(new_indices) && !new_indices){
    return(out)
  }
  out <- simplifyIndices(out, new_indices)
  out
}
