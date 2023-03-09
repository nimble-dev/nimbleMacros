# Functions copied from the nimble package

# Determine if a piece of code contains a '['
hasBracket <- function(code) {
    if(length(code) < 2) return(FALSE)
    if(code[[1]] == '[') return(TRUE)
    FALSE
}

embedLinesInCurlyBrackets <- function(lines) {
  as.call(c(list(quote(`{`)), lines))
}
