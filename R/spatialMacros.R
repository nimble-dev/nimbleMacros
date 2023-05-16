getICARInfo <- function(x, prefix, ...) UseMethod("getICARInfo")

getICARInfo.SpatialPolygons <- function(x, prefix, ...){
  W_nb <- poly2nb(x, row.names =  rownames(x@data))
  nbInfo <- nb2WB(W_nb)
  out <- list(nbInfo$adj, nbInfo$weights, length(nbInfo$adj), nbInfo$num)
  names(out) <- paste0(prefix, c("adj", "weights", "L", "num"))
  out
}

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
  getICARInfo.data.frame(as.data.frame(coordinates(x)), prefix=prefix, threshold=threshold)
}

#' @export
ICAR <- nimble::model_macro_builder(
function(stoch, LHS, spatData, threshold=NULL, prefix=quote(ICAR_), zero_mean=0, modelInfo, .env){
  spatData <- eval(spatData, envir=.env)
  idx <- extractIndices(LHS)[[1]]
  pars <- paste0(deparse(prefix), c("adj", "weights", "L", "num", "tau", "sigma"))
  names(pars) <- c("adj", "weights", "L", "num", "tau", "sigma")
  pars_names <- lapply(pars, str2lang)
  
  # New constants
  new_const <- getICARInfo(x=spatData, prefix=prefix, threshold=threshold)
  modelInfo$constants <- c(modelInfo$constants, new_const)

  icar_line <- substitute(LHS ~ dcar_normal(ADJ[1:L], WEIGHTS[1:L], NUM[IDX], TAU, zero_mean = ZEROMEAN),
                          list(LHS=LHS, ADJ=pars_names$adj, WEIGHTS=pars_names$weights,
                               NUM=pars_names$num, TAU=pars_names$tau, ZEROMEAN=zero_mean,
                               IDX=idx, L=pars_names$L))
  tau_lines <- substitute({
    TAU <- 1 / SIGMA^2
    SIGMA ~ dunif(0, 100)
  }, list(TAU=pars_names$tau, SIGMA=pars_names$sigma))
  
  out <- embedLinesInCurlyBrackets(list(icar_line, tau_lines))
  out <- removeExtraBrackets(out)

  #parameters <- c(parameters, list(priors=nimbleMacros:::findDeclarations(out)))
  list(code=out, modelInfo=modelInfo)
},
use3pieces=TRUE,
unpackArgs=TRUE)

#' @export
expcov <- nimbleFunction(     
  run = function(dists = double(2), rho = double(0), sigma = double(0)) {
    returnType(double(2))
    n <- dim(dists)[1]
    result <- matrix(nrow = n, ncol = n, init = FALSE)
    sigma2 <- sigma*sigma   # calculate once
    for(i in 1:n)
      for(j in 1:n)
        result[i, j] <- sigma2*exp(-dists[i,j]/rho)
    return(result)
})


getDistMat <- function(x, prefix) UseMethod("getDistMat")

getDistMat.matrix <- function(x, prefix){
  dists <- as.matrix(dist(x))
  dists <- dists / max(dists)
  out <- list(dists, rep(1, nrow(dists)))
  names(out) <- paste0(prefix, c("dists", "ones"))
  out
}

getDistMat.data.frame <- function(x, prefix){
  getDistMat(as.matrix(x), prefix)
}

getDistMat.SpatialPoints <- function(x, prefix){
  getDistMat(coordinates(x), prefix)
}

#' @export
GaussianProcess <- nimble::model_macro_builder(
function(stoch, LHS, spatData, prefix=quote(GP_), modelInfo, .env){  
  idx <- nimbleMacros:::extractIndices(LHS)[[1]]
  spatData <- eval(spatData, envir=.env)
  pars <- paste0(deparse(prefix), c("mu0","sigma","rho","mu","cov"))
  names(pars) <- c("mu0","sigma","rho","mu","cov")
  pars_names <- lapply(pars, str2lang)

  # New constants
  new_const <- getDistMat(spatData, prefix)
  modelInfo$constants <- c(modelInfo$constants, new_const)
  
  # Priors
  priors <- substitute({
    MU0 ~ dnorm(0, sd = 100)
    SIGMA ~ dunif(0, 100)
    RHO ~ dunif(0, 5)
  }, list(MU0=pars_names$mu0, SIGMA=pars_names$sigma, RHO=pars_names$rho))

  mu_line <- substitute(MU[IDX] <- MU0 * ONES[IDX],
                        list(MU=pars_names$mu, IDX=idx, MU0=pars_names$mu0,
                             ONES=str2lang(paste0(prefix, "ones"))))
  dist_name <- str2lang(paste0(prefix, "dists"))
  cov_line <- substitute(COV[IDX, IDX] <- SIGMA*SIGMA*exp(-DIST[IDX, IDX] / RHO),
                         list(COV=pars_names$cov, IDX=idx, DIST=dist_name,
                              RHO=pars_names$rho, SIGMA=pars_names$sigma))
  gp_line <- substitute(LHS ~ dmnorm(MU[IDX], cov = COV[IDX, IDX]),
                        list(LHS=LHS, MU=pars_names$mu, IDX=idx, COV=pars_names$cov))
  
  out <- nimbleMacros:::embedLinesInCurlyBrackets(list(priors, mu_line, cov_line, gp_line))
  out <- nimbleMacros:::removeExtraBrackets(out)

  #parameters <- c(parameters, list(priors=nimbleMacros:::findDeclarations(out)))
  list(code=out, modelInfo=modelInfo)
},
use3pieces=TRUE,
unpackArgs=TRUE)
