#' @export
occupancy <- nimble::model_macro_builder(
function(stoch, LHS, formula, statePrefix=quote(state_), detPrefix=quote(det_), 
         statePriors=setPriors(intercept="dunif(-10, 10)", coefficient="dlogis(0, 1)"), 
         detPriors=setPriors(intercept="dunif(-10, 10)", coefficient="dlogis(0, 1)"), 
         modelInfo, .env){

    formulas <- split_formula(formula)
  
    resp <- LHS
    site_idx <- LHS[[3]]
    obs_idx <- LHS[[4]]

    code <- substitute({
      psi[SITEIDX] <- linPred(STATEFORM, link=logit, coefPrefix=STATEPREFIX, sdPrefix=STATEPREFIX, priorSettings=STATEPRIOR)  
      p[SITEIDX, OBSIDX] <- linPred(DETFORM, link=logit, coefPrefix=DETPREFIX, sdPrefix=DETPREFIX, priorSettings=DETPRIOR)
  
      z[SITEIDX] ~ forLoop(dbern(psi[SITEIDX]))

      RESP ~ forLoop(dbern(p[SITEIDX, OBSIDX]*z[SITEIDX]))
      }, 
      list(RESP=resp, SITEIDX=site_idx, OBSIDX=obs_idx,
           STATEFORM=formulas[[2]], DETFORM = formulas[[1]],
           STATEPREFIX=statePrefix, DETPREFIX = detPrefix,
           STATEPRIOR=statePriors, DETPRIOR=detPriors)
    )

    # Return code and model info
    list(code=code, modelInfo=modelInfo)
  },
use3pieces=TRUE,
unpackArgs=TRUE
)
class(occupancy) <- "model_macro"

split_formula <- function(formula){
  if(length(formula) != 3) stop("Double right-hand side formula required")
  char <- lapply(formula, function(x){
            paste(deparse(x), collapse="")
          })
  p1 <- as.formula(char[[2]])
  p2 <- as.formula(paste("~", char[[3]]))
  list(p1, p2)
}

#' @export
nmixture <- nimble::model_macro_builder(
function(stoch, LHS, formula, statePrefix=quote(state_), detPrefix=quote(det_), 
         statePriors=setPriors(intercept="dunif(-10, 10)", coefficient="dnorm(0, 0.01)"), 
         detPriors=setPriors(intercept="dunif(-10, 10)", coefficient="dnorm(0, 0.01)"), 
         modelInfo, .env){

    formulas <- split_formula(formula)
  
    resp <- LHS
    site_idx <- LHS[[3]]
    obs_idx <- LHS[[4]]

    code <- substitute({
      lam[SITEIDX] <- linPred(STATEFORM, link=log, coefPrefix=STATEPREFIX, sdPrefix=STATEPREFIX, priorSettings=STATEPRIOR)  
      p[SITEIDX, OBSIDX] <- linPred(DETFORM, link=logit, coefPrefix=DETPREFIX, sdPrefix=DETPREFIX, priorSettings=DETPRIOR)
  
      N[SITEIDX] ~ forLoop(dpois(lam[SITEIDX]))

      RESP ~ forLoop(dbinom(N[SITEIDX], p[SITEIDX, OBSIDX]))
      }, 
      list(RESP=resp, SITEIDX=site_idx, OBSIDX=obs_idx,
           STATEFORM=formulas[[2]], DETFORM = formulas[[1]],
           STATEPREFIX=statePrefix, DETPREFIX = detPrefix,
           STATEPRIOR=statePriors, DETPRIOR=detPriors)
    )

    # Return code and model info
    list(code=code, modelInfo=modelInfo)
  },
use3pieces=TRUE,
unpackArgs=TRUE
)
class(nmixture) <- "model_macro"

#' @export
dynocc <- nimble::model_macro_builder(
function(stoch, LHS, psiformula, gammaformula, epsilonformula, pformula,
         psiPrefix=quote(psi_), gamPrefix=quote(gam_), epsPrefix=quote(eps_), 
         detPrefix=quote(det_), 
         statePriors=setPriors(intercept="dunif(-10, 10)", coefficient="dnorm(0, 0.01)"), 
         detPriors=setPriors(intercept="dunif(-10, 10)", coefficient="dnorm(0, 0.01)"), 
         modelInfo, .env){
  
    resp <- LHS
    site_idx <- LHS[[3]]
    obs_idx <- LHS[[4]]
    per_idx <- LHS[[5]]
    npers <- per_idx[[3]]

    code <- substitute({
      psi[SITEIDX] <- linPred(PSIFORM, link=logit, coefPrefix=PSIPREFIX, sdPrefix=PSIPREFIX, priorSettings=STATEPRIOR)
      gam[SITEIDX, 2:T] <- linPred(GAMFORM, link=logit, coefPrefix=GAMPREFIX, sdPrefix=GAMPREFIX, priorSettings=STATEPRIOR)
      eps[SITEIDX, 2:T] <- linPred(EPSFORM, link=logit, coefPrefix=EPSPREFIX, sdPrefix=EPSPREFIX, priorSettings=STATEPRIOR)  
      p[SITEIDX, OBSIDX, PERIDX] <- linPred(DETFORM, link=logit, coefPrefix=DETPREFIX, sdPrefix=DETPREFIX, priorSettings=DETPRIOR)
  
      z[SITEIDX, 1] ~ forLoop(dbern(psi[SITEIDX]))
      
      for (n in SITEIDX){
        for (t in 2:T){
          z[n, t] ~ dbern(z[n, t-1]*(1 - eps[n, t]) + (1-z[n, t-1]) * gam[n, t])
        }
      }

      RESP ~ forLoop(dbern(z[SITEIDX,PERIDX] * p[SITEIDX, OBSIDX, PERIDX]))
      }, 
      list(RESP=resp, SITEIDX=site_idx, OBSIDX=obs_idx, PERIDX=per_idx, T=npers,
           PSIFORM=psiformula, GAMFORM=gammaformula, EPSFORM=epsilonformula, DETFORM = pformula,
           PSIPREFIX=psiPrefix, GAMPREFIX=gamPrefix, EPSPREFIX=epsPrefix, DETPREFIX = detPrefix,
           STATEPRIOR=statePriors, DETPRIOR=detPriors)
    )

    # Return code and model info
    list(code=code, modelInfo=modelInfo)
  },
use3pieces=TRUE,
unpackArgs=TRUE
)
class(dynocc) <- "model_macro"
