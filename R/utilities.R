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
