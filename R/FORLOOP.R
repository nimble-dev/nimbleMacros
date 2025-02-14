#' Macro to build for loop(s) from code with index ranges in brackets
#'
#' This macro takes a line of NIMBLE model code with index ranges inside brackets
#' on either the left-hand side of a declaration or both the left- and
#' right-hand sides of a declaration and constructs a corresponding
#' for loop or series of nested for loops.
#'
#' @name FORLOOP
#'
#' @author Ken Kellner and Perry de Valpine
#'
#' @param code The right-hand side of a parameter declaration
#'
#' @examples
#' code <- nimbleCode({
#'    y[1:n, 1:2, 1] ~ FORLOOP(dnorm(mu[1:n], sigma))
#'    mu[1:n] <- FORLOOP(beta[1] + beta[2]*x[1:n])
#' })
#'
#' mod <- nimbleModel(code, constants = list(n=10))
#' mod$getCode()
NULL

#' @export
FORLOOP <- nimble::buildMacro(
function(code, modelInfo, .env){
  # Remove FORLOOP from the code line
  code <- removeMacroCall(code)
  LHS <- getLHS(code)
  # Stop if there are no brackets, we can't do a for loop in that case
  if(!hasBracket(LHS)) return(list(code=code, modelInfo = modelInfo))
  # Check if there are duplicate index ranges in bracket on LHS
  # If there are give an error
  checkDuplicateBracketComponents(LHS)
  # Extract indices from brackets
  idx <- extractIndices(LHS)
  # Check if they are index ranges
  has_range <- isIndexRange(idx)
  # Stop if none of the indices are ranges
  if(all(!has_range)) return(list(code=code, modelInfo = modelInfo))

  # Subset only indexes with ranges
  idx_sub <- idx[has_range]
  # Create new index letters to represent those ranges in the for loops
  idx_letters <- lapply(1:length(idx_sub), function(i) modelInfo$indexCreator())
  idx_letters <- lapply(idx_letters, as.name)
  # Replace the ranges with the new corresponding letters
  code <- replaceDeclarationIndexRanges(code, idx_letters)
  # Then insert the ranges into the for loop structure
  idx_sub <- replaceRanges(idx_sub, idx_letters)
  # So e.g.
  # FORLOOP(quote(x[1:10] <- y[1:10])) would be
  # for (i_1 in 1:10){ # <- index range inserted here
  #   x[i_1] <- y[i_1] # <- index range replaced with index letter
  # }

  # Create for loop structure of arbitrary nestedness
  # substituting in info from above
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

  return(list(code=code, modelInfo=modelInfo))
},
use3pieces=FALSE,
unpackArgs=FALSE
)


# Check if code chunk has bracket
hasBracket <- function(code, recursive=TRUE){
  if (length(code) < 2) return(FALSE)
  if (code[[1]] == "[") return(TRUE)
  if(recursive){
    if (is.name(code[[1]])) return(hasBracket(code[[2]]))
  }
    FALSE
}

# Extract bracket and contents
getBracket <- function(code){
  if(!hasBracket(code)) stop("Code should have bracket")
  if(code[[1]] == "[") return(code)
  if(is.name(code[[1]])) return(getBracket(code[[2]]))
}

# Get index values from bracket
# For example alpha[1,1:10,k] returns list(1, 1:10, k)
extractIndices <- function(code){
    if(!hasBracket(code)) stop("Code should have bracket")
    code <- getBracket(code)
    out <- lapply(3:length(code), function(x) code[[x]])

    out <- lapply(out, function(x){
      if(hasBracket(x)) return(extractIndices(x))
      else(x)
    })
    out <- unlist(out) # possibly not safe?
    out <- unique(out) # possibly not safe?

    out
}

# Determine which indices in a set from a bracket are ranges
isIndexRange <- function(code){
  sapply(code, function(x) any(grepl(":",x)))
}

# Check if any code component is an index range (kind of trivial function)
anyIndexRange <- function(code){
  any(isIndexRange(code))
}

# Remove the macro part of a line
# Only works for assignments where macro is on RHS
# Not sure if need to make this more general in the future
removeMacroCall <- function(code){
  if(!isAssignment(code)) stop("Code should have assignment")
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

# Remove index offsets/adjustments
# For example if idx = quote(1:3 - 1), function returns quote(1:3)
removeIndexAdjustments <- function(idx){
  if(length(idx) == 1) return(idx) 
  if(idx[[1]] == ":") return(idx)
  has_range <- which(sapply(idx, function(x){
    if(!is.call(x)) return(FALSE)
    if(is.name(x)) return(FALSE)
    if(x[[1]] == ":") return(TRUE)
    FALSE
  }))
  idx[[has_range]]
}

# Check if index range on both sides of declaration are the same
# Specifically, all indices on RHS have to be present in LHS
hasMatchingIndexRanges <- function (LHS, RHS){
    idx_LHS <- extractIndices(LHS)
    idx_LHS <- idx_LHS[isIndexRange(idx_LHS)]
    idx_LHS <- lapply(idx_LHS, removeIndexAdjustments)
    idx_RHS <- extractAllIndices(RHS)
    idx_RHS <- idx_RHS[isIndexRange(idx_RHS)]
    idx_RHS <- lapply(idx_RHS, removeIndexAdjustments)
    all(idx_RHS %in% idx_LHS)
}

# Check if code has index adjustment (only looks for - and + currently)
# So hasAdjustment(quote(1:3)) is FALSE but hasAdjustment(quote(1:3 - 1)) is TRUE
hasAdjustment <- function(code){
  if(is.name(code)) return(FALSE)
  if(length(code) < 3) return(FALSE)
  code[[1]] == "-" | code[[1]] == "+"
}

# Replace a provided index in some code with a new value
replaceIndex <- function(code, old_idx, new_idx){
  code_list <- as.list(code)
  code_list <- lapply(code_list, function(x){
    if(hasBracket(x)){
      return(replaceIndex(x, old_idx, new_idx))
    }
    if(hasAdjustment(x)){
      return(replaceIndex(x, old_idx, new_idx))
    }
    x
  })
  idx <- which(code_list == old_idx)
  # If old index is not found do nothing
  if(length(idx) == 0) return(as.call(code_list))
  if(length(idx) > 1) stop("Can't handle duplicate indices")
  code_list[[idx]] <- new_idx
  as.call(code_list)
}

# Recursive version of replaceIndex
# Typically used to handle RHS where there are nested lists in the call
recursiveReplaceIndex <- function(code, old_idx, new_idx){
  if(hasBracket(code) && code[[1]] == "["){
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
  if(!hasMatchingIndexRanges(LHS, RHS)) stop("Index ranges must match")
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

# Handle situation when idx range itself contains another idx range
# like 1:len[1:N]
replaceRanges <- function(ranges, idx_letters){
  for (i in 1:length(ranges)){
    idx_has_bracket <- any(sapply(as.list(ranges[[i]]), hasBracket)) 
    if(idx_has_bracket){
      other_ranges <- ranges[-i]
      idx_letters_sub <- idx_letters[-i]
      for (j in 1:length(other_ranges)){
        ranges[[i]] <- recursiveReplaceIndex(ranges[[i]], other_ranges[[j]], idx_letters_sub[[j]])
      }
    }
  }
  ranges
}

# Check if any of the index ranges in bracket expression are duplicates: 
# if they are, we can't handle them because matching them to indices on the RHS
# could be ambiguous
# For example y[1:M, 1:M] won't work but y[1:M, sind[1:M]] will
checkDuplicateBracketComponents <- function(code){
  if(!hasBracket(code)) stop("Code should have bracket")
  code <- getBracket(code)
  out <- lapply(3:length(code), function(x) code[[x]])
  out <- out[sapply(out, anyIndexRange)]
  dups <- duplicated(out)
  if(any(dups)){
    stop("Not sure how to expand duplicated index ",
         safeDeparse(out[dups][[1]])," in code:\n", safeDeparse(code),
         call.=FALSE)
  }
  invisible()
}
