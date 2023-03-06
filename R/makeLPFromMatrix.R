getLPUnit <- function(par, mat, ind, ss){
  par <- substitute(PAR[IND], list(PAR=par, IND=ind))
  dat <- substitute(MAT[SS, IND], list(MAT=mat, SS=ss, IND=ind))
  substitute(PAR * DAT, list(PAR=par, DAT=dat))
}

#' @export
makeLPFromMatrix <- list(
  process = function(code, .constants, .env){
    RHS <- getRHS(code)
    ss <- extractIndices(RHS[[2]])[[1]]
    par_range <- extractIndices(RHS[[2]])[[2]]
    #par_range <- substitute(par_range, .constants)
    par_range[[3]] <- .constants$p  # need to not hardcode this
    par_range <- as.numeric(eval(par_range))
    np <- length(par_range)
    par_name <- RHS[[3]][[2]]
    mat <- RHS[[2]][[2]]

    if(length(RHS) == 4){
      link <- RHS[[4]]
      LHS <- getLHS(code)
      LHS <- as.call(list(link, LHS))
      LHS(code) <- LHS
    }

    out <- getLPUnit(par_name, mat, as.numeric(par_range[1]), ss)
    for (i in 2:np){
      next_lp <- getLPUnit(par_name, mat, as.numeric(par_range[i]), ss)
      out <- as.call(list(quote(`+`), out, next_lp))
    }
    out <- as.call(list(quote(buildLoop), out))
    RHS(code) <- out
    list(code=code)
  }
)
class(makeLPFromMatrix) <- "model_macro"
