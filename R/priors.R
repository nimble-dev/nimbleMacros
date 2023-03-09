#' @export
priors <- list(process=function(code, .constants, .env=env){
  form <- nimbleMacros:::getRHS(code)[1:2]
  if(form[[1]] != quote(`~`)) form[[1]] <- quote(`~`)
  form <- as.formula(form)

  newConstants <- nimbleMacros:::splitFactorsInConstants(.constants)
  mm_cols <- nimbleMacros:::modelMatrixCols(form, newConstants)
  
  prefix <-nimbleMacros:::getLHS(code)
  pars <- nimbleMacros:::getParametersForLP(mm_cols, prefix=prefix)

  priors <- nimbleMacros:::getRHS(code)[[3]]
  
  priors_code <- lapply(pars, function(x, priors){
    substitute(RHS ~ LHS, list(RHS=str2lang(x), LHS=priors))
    }, priors=priors)

  list(code=embedLinesInCurlyBrackets(priors_code), constants=.constants)
})
class(priors) <- "model_macro"
