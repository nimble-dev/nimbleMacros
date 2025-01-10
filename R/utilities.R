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
