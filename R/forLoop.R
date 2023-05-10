hasBracket <- function(code, recursive=TRUE){
  if (length(code) < 2) return(FALSE)
  if (code[[1]] == "[") return(TRUE)
  if(recursive){
    if (is.name(code[[1]])) return(hasBracket(code[[2]]))
  }
    FALSE
}

getBracket <- function(code){
  stopifnot(hasBracket(code))
  if(code[[1]] == "[") return(code)
  if(is.name(code[[1]])) return(getBracket(code[[2]]))
}

# Is chunk of code assigning a value or distribution to LHS
# declaration
isAssignment <- function(code){
  as.character(code[[1]]) %in% c("~", "<-")
}

# If "assignment" get LHS
getLHS <- function(code){
  stopifnot(isAssignment(code))
  code[[2]]
}

# If "assignment" get RHS
getRHS <- function(code){
  stopifnot(isAssignment(code))
  code[[3]]
}

# Replace LHS of assignment code chunk
`LHS<-` <- function(code, value){
  stopifnot(isAssignment(code))
  stopifnot(is.call(value)|is.name(value))
  code[[2]] <- value
  code
}

# Replace RHS of assignment code chunk
`RHS<-` <- function(code, value){
  stopifnot(isAssignment(code))
  stopifnot(is.call(value)|is.name(value)|is.numeric(value))
  code[[3]] <- value
  code
}


# Get index values from bracket
# For example alpha[1,1:10,k] returns list(1, 1:10, k)
extractIndices <- function(code){
    stopifnot(hasBracket(code))
    code <- getBracket(code)
    out <- lapply(3:length(code), function(x) code[[x]])
    if (hasBracket(out[[1]])) {
        return(extractIndices(out[[1]]))
    }
    out
}

# Determine which indices in a set from a bracket are ranges
isIndexRange <- function(code){
  sapply(code, function(x) any(grepl(":",x)))
}


anyIndexRange <- function(code){
  any(isIndexRange(code))
}

# Remove the macro part of a line
# Only works for assignments where macro is on RHS
# Not sure if need to make this more general in the future
removeMacroCall <- function(code){
  stopifnot(isAssignment(code))
  ind <- 2:length(getRHS(code))
  repl <- getRHS(code)[ind]
  if(length(repl) == 1) repl <- repl[[1]]
  RHS(code) <- repl
  code
}

# Extract all indices from code
# Should be passed either LHS or RHS of declaration
extractAllIndices <- function(code){
  if(hasBracket(code, recursive=FALSE)){
    out <- extractIndices(code)
  } else{
    if(is.call(code)){
      out <- lapply(code, extractAllIndices)
    } else {
      out <- NULL
    }
  }
  out <- unlist(out)
  if(is.call(out) | is.numeric(out)) out <- list(out) # always return a list
  return(out)
}

# Check if index range on both sides of declaration are the same
# Specifically, all indices on RHS have to be present in LHS
hasMatchingIndexRanges <- function(LHS, RHS){
  idx_LHS <- extractIndices(LHS)
  idx_LHS <- idx_LHS[isIndexRange(idx_LHS)]
  idx_RHS <- extractAllIndices(RHS)
  idx_RHS <- idx_RHS[isIndexRange(idx_RHS)]
  all(idx_RHS %in% idx_LHS)
  #all(mapply(function(x, y) x==y, idx_LHS, idx_RHS))
}

# Replace a provided index in some code with a new value
replaceIndex <- function(code, old_idx, new_idx){
  stopifnot(hasBracket(code))
  code_list <- as.list(code)
  code_list <- lapply(code_list, function(x){
    if(hasBracket(x)){
      return(replaceIndex(x, old_idx, new_idx))
    }
    x
  })
  idx <- which(code_list == old_idx)
  # If old index is not found do nothing
  if(length(idx) != 1) return(as.call(code_list))
  code_list[[idx]] <- new_idx
  as.call(code_list)
}

# Recursive version of replaceIndex
# Typically used to handle RHS where there are nested lists in the call
recursiveReplaceIndex <- function(code, old_idx, new_idx){
  if(hasBracket(code)){
    out <- replaceIndex(code, old_idx, new_idx)
  } else{
    if(is.call(code)){
      out <- lapply(code, recursiveReplaceIndex, old_idx=old_idx, new_idx=new_idx)
    } else {
      out <- code
    }
  }
  if(!is.name(out)&!is.numeric(out)) out <- as.call(out)
  out
}

# Replace all indices on both sides LHS/RHS
replaceDeclarationIndexRanges <- function(code, new_idx_list){
  LHS <- getLHS(code)
  RHS <- getRHS(code)
  op <- code[[1]]
  stopifnot(hasMatchingIndexRanges(LHS, RHS))
  idx <- extractAllIndices(LHS)
  idx <- idx[isIndexRange(idx)]
  if(length(idx) == 0) return(code)
  nidx <- length(idx)
  for (i in 1:nidx){
    LHS <- recursiveReplaceIndex(LHS, idx[[i]], new_idx_list[[i]])
    RHS <- recursiveReplaceIndex(RHS, idx[[i]], new_idx_list[[i]])
  }
  return(as.call(list(op, LHS, RHS)))
}


#' Macro to build for loop(s) from code with index ranges in brackets
#'
#' This macro takes a line of BUGS code with index ranges inside brackets
#' on either the left-hand side of a declaration or both the left- and
#' right-hand sides of a declaration and constructs a corresponding
#' for loop or series of nested for loops.
#'
#' @name forLoop
#'
#' @author Ken Kellner
#'
#' @param code The right-hand side of a parameter declaration
#'
#' @examples
#' \donttest{
#' code <- nimbleCode({
#'   y[1:n, 1:2, 1] ~ forLoop(dnorm(mu[1:n], sigma))
#'   mu[1:n] <- forLoop(beta[1] + beta[2]*x[1:n])
#' })
#' nimble:::codeProcessModelMacros(code)
#' }
NULL

#' @export
forLoop <- list(
  process = function(code, .constants, parameters=list(), .env, indexCreator){
    code <- removeMacroCall(code)
    LHS <- getLHS(code)
    # Stop if there are no brackets
    if(!hasBracket(LHS)) return(list(code=code, constants=.constants))
    idx <- extractIndices(LHS)
    has_range <- isIndexRange(idx)
    # Stop if none of the indices are ranges
    if(all(!has_range)) return(list(code=code, constants=.constants))

    idx_sub <- idx[has_range]
    #idx_letters <- paste0(c(letters[9:12], letters[-c(9:12)]),"_")
    idx_letters <- lapply(1:length(idx_sub), function(i) indexCreator())
    idx_letters <- lapply(idx_letters, as.name)

    code <- replaceDeclarationIndexRanges(code, idx_letters)

    for(i in length(idx_sub):1) {
      newForLoop <-
        substitute(
          for(NEWINDEX_ in RANGE_){
            INNERCODE
          },
          list(NEWINDEX_ = idx_letters[[i]],
               RANGE_ = idx_sub[[i]],
               INNERCODE = code))
      code <- newForLoop
    }

    return(list(code=code, constants=.constants, parameters=parameters))
  }
)
class(forLoop) <- "model_macro"
