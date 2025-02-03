#' Function to handle ICAR() in LINPRED
#'
#' Translates ICAR() in an R formula passed to LINPRED into corresponding
#' NIMBLE code for an ICAR spatial random effect. This is used internally
#' by \code{LINPRED} and should not be called directly. New formula functions
#' should have the same arguments, naming structure, class (\code{nimbleFormulaHandler}) 
#' and return object class (\code{formulaComponent}).
#'
#' @param x A \code{formulaComponentFunction} object created from an ICAR() term
#' @param defaultBracket The bracket from the LHS of LINPRED
#' @param coefPrefix The prefix to use for any new linear predictor parameters created
#' @param sdPrefix The prefix to use for any new standard deviation parameters created
#' @param modelInfo Named list containing model information including constants
#' @param env Environment in which the LINPRED macro was called
#' @param ... Not currently used
#'
#' @return An object of class \code{formulaComponent}.
#'
#' @author Ken Kellner
#'
#' @export
formulaHandler_ICAR <- function(x, defaultBracket, coefPrefix, sdPrefix, modelInfo, env, ...){

  # Extract any brackets
  brack <- extractAllBrackets(x$lang[[2]])
  if(is.null(brack)) brack <- defaultBracket
  if(length(brack) > 1) stop("Can't handle multiple brackets inside ICAR()", call.=FALSE)

  lp_term <- str2lang(paste0(coefPrefix, "ICAR", brack))
  rhs <- str2lang(paste0("nimbleMacros::", safeDeparse(x$lang)))

  prior <- substitute(LHS ~ RHS, list(LHS = lp_term, RHS = rhs))
  
  x$linPredCode <- lp_term
  x$priorCode <- prior
  x
}
class(formulaHandler_ICAR) <- c(class(formulaHandler_ICAR), "nimbleFormulaHandler")


#' Macro to build code for an ICAR random effect
#'
#' Takes spatial data as a Spatial object or data frame of coordinates, and
#' generates corresponding code and constants for an intrinsic conditional 
#' auto-regressive (ICAR) random effect
#'
#' @name ICAR
#' @author Ken Kellner
#' 
#' @param spatData Spatial data object; can be SpatialPoints, SpatialPolygons,
#'  or data frame of coordinates
#' @param threshold Distance at and below which two points will be considered
#'  neighbors
#' @param prefix All macro-generated parameters will begin with this prefix,
#'  default is ICAR_ (so x becomes ICAR_x)
#' @param priorSpecs List of prior specifications, should be generated using 
#'  setPriors()
#' @param zero_mean Value passed to corresponding argument in dcar_normal() 
#'  distribution in NIMBLE code
#'
NULL

#' @export
ICAR <- nimble::buildMacro(
function(stoch, LHS, spatData, threshold=NULL, prefix=quote(ICAR_), priorSpecs = setPriors(),
         zero_mean=0, modelInfo, .env){
  spatData <- eval(spatData, envir=.env)
  idx <- extractIndices(LHS)[[1]]
  pars <- paste0(deparse(prefix), c("adj", "weights", "L", "num", "tau", "sigma"))
  names(pars) <- c("adj", "weights", "L", "num", "tau", "sigma")
  pars_names <- lapply(pars, str2lang)
  
  # New constants
  new_const <- getICARInfo(x=spatData, prefix=prefix, threshold=threshold)
  modelInfo$constants <- c(modelInfo$constants, new_const)

  # Prior settings
  priorSpecs <- eval(priorSpecs, envir=.env)
  sdPrior <- matchPrior(pars_names$sigma, "sd", priorSpecs = priorSpecs) 

  icar_line <- substitute(LHS ~ dcar_normal(ADJ[1:L], WEIGHTS[1:L], NUM[IDX], TAU, zero_mean = ZEROMEAN),
                          list(LHS=LHS, ADJ=pars_names$adj, WEIGHTS=pars_names$weights,
                               NUM=pars_names$num, TAU=pars_names$tau, ZEROMEAN=zero_mean,
                               IDX=idx, L=pars_names$L))
  tau_lines <- substitute({
    TAU <- 1 / SIGMA^2
    SIGMA ~ SIGPRIOR
  }, list(TAU=pars_names$tau, SIGMA=pars_names$sigma, SIGPRIOR=sdPrior))
  
  out <- embedLinesInCurlyBrackets(list(icar_line, tau_lines))
  out <- removeExtraBrackets(out)

  #parameters <- c(parameters, list(priors=nimbleMacros:::findDeclarations(out)))
  list(code=out, modelInfo=modelInfo)
},
use3pieces=TRUE,
unpackArgs=TRUE)

getICARInfo <- function(x, prefix, ...) UseMethod("getICARInfo")

#' @exportS3Method NULL
getICARInfo.SpatialPolygons <- function(x, prefix, ...){
  if(!requireNamespace("spdep", quietly=TRUE)) stop("Package spdep required", call.=FALSE)
  W_nb <- spdep::poly2nb(x, row.names =  rownames(x@data))
  nbInfo <- spdep::nb2WB(W_nb)
  out <- list(nbInfo$adj, nbInfo$weights, length(nbInfo$adj), nbInfo$num)
  names(out) <- paste0(prefix, c("adj", "weights", "L", "num"))
  out
}
.S3method("getICARInfo", "SpatialPolygons", getICARInfo.SpatialPolygons)

#' @exportS3Method NULL
getICARInfo.matrix <- function(x, prefix, threshold, ...){
  stopifnot(ncol(x) == 2)

  distmat <- as.matrix(stats::dist(x))

  A = matrix(0, nrow(distmat), ncol(distmat))
  A[distmat <= threshold] <- 1
  diag(A) <- 0
  
  nbInfo <- nimble::as.carAdjacency(A)
  out <- list(nbInfo$adj, nbInfo$weights, length(nbInfo$adj), nbInfo$num)
  names(out) <- paste0(prefix, c("adj", "weights", "L", "num"))
  out
}
.S3method("getICARInfo", "matrix", getICARInfo.matrix)

#' @exportS3Method NULL
getICARInfo.data.frame <- function(x, prefix, threshold, ...){
  getICARInfo(as.matrix(x), prefix=prefix, threshold=threshold, ...)
}
.S3method("getICARInfo", "data.frame", getICARInfo.data.frame)

#' @exportS3Method NULL
getICARInfo.SpatialPoints <- function(x, prefix, threshold, ...){
  if(!requireNamespace("sp", quietly=TRUE)) stop("Package sp required", call.=FALSE)
  getICARInfo.data.frame(as.data.frame(sp::coordinates(x)), prefix=prefix, threshold=threshold)
}
.S3method("getICARInfo", "SpatialPoints", getICARInfo.SpatialPoints)
