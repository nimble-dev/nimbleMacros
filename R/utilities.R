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

# Function copied from the nimble package
embedLinesInCurlyBrackets <- function(lines) {
  as.call(c(list(quote(`{`)), lines))
}

# Stolen from nimble; avoid splitting output into multiple lines
safeDeparse <- function(..., warn = FALSE) {
    out <- deparse(...)
    if(nimbleOptions('useSafeDeparse')) {
        dotArgs <- list(...)
        if("nlines" %in% names(dotArgs))
            nlines <- dotArgs$nlines else nlines <- 1L
        if(nlines != -1L && length(out) > nlines) {
            if(warn)
                message("  [Note] safeDeparse: truncating deparse output to ", nlines, " line", if(nlines>1) "s" else "")
            out <- out[1:nlines]
        }
    }
    return(out)
}
