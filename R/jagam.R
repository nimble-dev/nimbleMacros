#' @export
nimbleJagam <- nimble::model_macro_builder(
  function(formula, family=gaussian, data, weights=NULL, 
              offset=NULL, knots=NULL, sp=NULL, sp.prior="gamma",
              diagonalize = FALSE, modelInfo, .env){

  formula <- as.formula(formula)
  
  dat <- as.data.frame(modelInfo$constants)
  jags.file <- tempfile()
  mod <- mgcv::jagam(formula,data=dat, file=jags.file, weights=weights,
                 offset=offset, knots=knots, sp=sp, 
                 sp.prior=sp.prior,diagonalize=diagonalize)
  modtext <- suppressWarnings(nimble:::processModelFile(jags.file))
  modtext <- nimble:::mergeMultiLineStatements(modtext$modelLines)
  modtext <- nimble:::processNonParseableCode(modtext)

  # Fix missing brackets
  modtext <- sapply(modtext, add_missing_brackets, USE.NAMES=FALSE)
  out <- parse(text = modtext)[[1]]

  # Manually add brackets to first line
  #out[[2]] <- quote(mu[] <- X[,] %*% b[])

  #for (i in names(mod$jags.data)){
  #  modelInfo$constants[[i]] <- mod$jags.data[[i]]
  #}

  modelInfo$constants <- mod$jags.data
  modelInfo$inits <- mod$jags.ini # save inits into modelinfo object (good idea?)
  nb <- length(mod$jags.ini$b)

  Spar <- grepl("S[0-9]*", names(mod$jags.data))
  Spar <- names(mod$jags.data)[Spar]
  Kpar_dim <- lapply(Spar, function(x) rep(dim(mod$jags.data[[x]])[1], 2))
  names(Kpar_dim) <- gsub("S", "K", Spar)

  modelInfo$dimensions <- c(list(mu = mod$jags.data$n, b = nb, 
                               X = c(mod$jags.data$n, nb)), Kpar_dim)
    
  list(code=out, modelInfo=modelInfo)
},
use3pieces=FALSE, unpackArgs=TRUE)

add_missing_brackets <- function(x){
  x <- gsub("mu ", "mu[] ", x, fixed=TRUE)
  x <- gsub("b ", "b[] ", x, fixed=TRUE)
  x <- gsub("X ", "X[,] ", x, fixed=TRUE)

  pat <- "K[0-9]*"
  kval <- regmatches(x, regexpr(pat, x))
  kval <- paste0(kval, "[,]")
  x <- gsub("K[0-9]*", kval, x)
  x
}


# Convert to mcarray

#' @export
as.mcarray <- function(x){
  x <- as.array(x)
  x <- aperm(x, c(2,3,1))
  noind <- sapply(strsplit(rownames(x), "[", fixed=TRUE), function(x) x[1])
  unique_pars <- unique(noind)

  mc <- lapply(unique_pars, function(i){
    xsub <- x[noind == i,,]
    class(xsub) <- "mcarray"
    xsub
  })
  names(mc) <- unique_pars
  mc
}

#' @export
nimble2gam <- function(code, data, samples){ 
  cl <- as.list(code)
  ind <- which(sapply(cl, function(x){
           if(length(x) == 1) return(FALSE)
           deparse(x[[1]]) == "nimbleJagam"
  }))
  jagam_code <- cl[[ind]]
  jagam_code[[1]] <- quote(mgcv::jagam)
  jagam_code$data <- quote(data)
  jagam_code$file <- quote(tempfile())
  jd <- eval(jagam_code)
  
  nsam <- as.mcarray(samples)
  stopifnot(identical(names(nsam), c("b", "lambda", "tau")))
  names(nsam) <- c("b", "rho", "scale")
  mgcv::sim2jam(nsam, jd$pregam)
}

