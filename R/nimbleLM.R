#' @export
nimbleLM <- list(process = function(code, .constants, .env){
  
  RHS <- getRHS(code)
  LHS <- getLHS(code)
  idx <- extractIndices(LHS)[[1]]
  form <- RHS[[2]]
  priorLP <- if(is.null(RHS$priorLP)) quote(dnorm(0, sd=100)) else RHS$priorLP
  priorSig <- if(is.null(RHS$priorSig)) quote(dunif(0, 100)) else RHS$priorSig
  prefix <- if(is.null(RHS$prefix)) quote(beta.) else RHS$prefix
  
  dataDec <- substitute(DAT ~ forLoop(dnorm(mu[IDX], sd=sigma)),
                        list(DAT=LHS, IDX=idx))
  LP <- substitute(mu[IDX] <- linPred(FORM, prefix=PREFIX),
                   list(IDX=idx, FORM=form, PREFIX=prefix))
  LPprior <- substitute(PREFIX ~ priors(FORM, PRIORLP),
                        list(PREFIX=prefix, FORM=form, PRIORLP=priorLP))
  sigprior <- substitute(sigma ~ PRIORSIG, list(PRIORSIG=priorSig))
  out <- list(dataDec, LP, LPprior, sigprior)
  list(code=embedLinesInCurlyBrackets(out), constants=.constants)
})
class(nimbleLM) <- 'model_macro'
