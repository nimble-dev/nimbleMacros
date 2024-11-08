# Add spatial random effect term to linear predictor if needed
addSpatialToLP <- function(code, coefPrefix, spatialModel = NULL, 
                           spatialIndex = NULL){
  # If no spatial model do nothing
  if(is.null(spatialModel)){
    return(code)
  }
  
  # Check supported models
  mod_name <- safeDeparse(spatialModel[[1]])
  supported_models <- c("ICAR")
  if(!mod_name %in% supported_models){
    stop("Spatial model must be one of the following: ", paste(supported_models, collapse=", "), call.=FALSE)
  }

  # Add spatial term at end of linear predictor
  spatial_re <- str2lang(paste0(coefPrefix, mod_name))
  spatial_re <- substitute(SPAT[IDX], list(SPAT=spatial_re, IDX=spatialIndex))
  out <- substitute(LP + SPAT, list(LP=code, SPAT=spatial_re))
  out
}

# Add spatial random effect to priors if needed
addSpatialToPriors <- function(code, coefPrefix, spatialModel = NULL,
                               spatialIndex = NULL){

  # If no spatial model do nothing
  if(is.null(spatialModel)){
    return(code)
  }

  mod_name <- safeDeparse(spatialModel[[1]])

  # Add nimbleMacros::
  # below sets off check warning
  #spatialModel[[1]] <- substitute(nimbleMacros::MOD, list(MOD = spatialModel[[1]]))
  spatialModel[[1]] <- str2lang(paste0("nimbleMacros::", safeDeparse(spatialModel[[1]])))

  # Manually set prefix for spatial parameter terms
  spatialModel$prefix <- str2lang(paste0(coefPrefix, mod_name, "_"))

  spatial_re <- str2lang(paste0(coefPrefix, mod_name))
  spatial_re <- substitute(SPAT[IDX], list(SPAT=spatial_re, IDX=spatialIndex))
  
  spat_code <- substitute(SPATRE ~ SPATMOD,
                          list(SPATRE = spatial_re, SPATMOD = spatialModel))

  embedLinesInCurlyBrackets(list(code, spat_code))
}

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

getICARInfo.SpatialPolygons <- function(x, prefix, ...){
  if(!requireNamespace("spdep", quietly=TRUE)) stop("Package spdep required", call.=FALSE)
  W_nb <- spdep::poly2nb(x, row.names =  rownames(x@data))
  nbInfo <- spdep::nb2WB(W_nb)
  out <- list(nbInfo$adj, nbInfo$weights, length(nbInfo$adj), nbInfo$num)
  names(out) <- paste0(prefix, c("adj", "weights", "L", "num"))
  out
}

#' @importFrom stats dist
getICARInfo.data.frame <- function(x, prefix, threshold, ...){
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

getICARInfo.SpatialPoints <- function(x, prefix, threshold, ...){
  if(!requireNamespace("sp", quietly=TRUE)) stop("Package sp required", call.=FALSE)
  getICARInfo.data.frame(as.data.frame(sp::coordinates(x)), prefix=prefix, threshold=threshold)
}
