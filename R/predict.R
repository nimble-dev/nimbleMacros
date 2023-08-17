calcParOrigData <- function(lpcode, LHS, env){
  parshort <- LHS[[2]]
  LHS_dim <- as.list(LHS[3:length(LHS)])
  LHS_dim <- sapply(LHS_dim, function(x){
    with(env, length(eval(x)))
  })
  eval(substitute(PAR <- array(NA, dim=DIM),
                    list(PAR=parshort, DIM=LHS_dim)))
  res <- within(env,
                eval(lpcode))[[deparse(parshort)]]
  drop(res)
}

calcParNewdata <- function(lpcode, LHS, env){
  eval(lpcode, envir=env)
}

getLPForNewdata <- function(LHS, RHS, data, prefix){
  LHS_ind <- extractIndices(LHS)
  dd <- makeDummyDataFrame(RHS, data)
  lp <- makeLPFromFormula(RHS, dd, LHS_ind, prefix)
  flattenBrackets(lp)
}

getLPForOrigData <- function(LHS, RHS, data, prefix){
  LHS_dim <- as.list(LHS[3:length(LHS)])
  LHS_dim <- sapply(LHS_dim, function(x){
    with(data, length(eval(x)))
  })

  code <- substitute(PAR <- linPred(FORM, coefPrefix=PREFIX, priorSettings=NULL),
                   list(PAR=LHS, FORM=RHS, PREFIX=prefix))

  modinfo <- list(constants=data, 
                  indexCreator=nimble:::labelFunctionCreator("i"))

  lp <- linPred$process(code, modinfo, environment())
  lp <- forLoop$process(lp$code, lp$modelInfo, environment())
  lp
}

flattenBrackets <- function(code){
  if(is.call(code) & as.list(code)[[1]] == "["){
    flattenBracketsInternal(code)
  } else if(is.call(code)){
    return(as.call(lapply(as.list(code), flattenBrackets)))
  } else {
    return(code)
  }
}

flattenBracketsInternal <- function(x){
  if(x[[3]][[1]] != "["){
    x <- x[1:2]
    return(as.call(x))
  }


  for (i in 3:length(x)){
    if(x[[i]][[1]] == "["){
      x[[i]] <- x[[i]][1:2]
    }
  }

  x[[3]] <- as.call(c(list(quote(cbind)), as.list(x[3:length(x)])))
  
  if(length(as.list(x)) > 3){
    for(i in 4:length(as.list(x))){
      x[[i]] <- NULL
    }
  }
  
  x
}

#' @export
predictLinPred <- function(samples, LHS, RHS, prefix, data, invlink=NULL, newdata=NULL){

  nsamp <- nrow(as.matrix(samples))
  bnames <- colnames(samples[[1]])
  bnames <- bnames[grepl(prefix, bnames)]
  simlist <- sims_list(samples[,bnames])

  if(is.null(newdata)){
    lp <- getLPForOrigData(LHS, RHS, data, str2lang(prefix))
    inpdata <- lp$modelInfo$constants
    lp <- lp$code
    evalfun <- calcParOrigData
  } else{
    lp <- getLPForNewdata(LHS, RHS, data, str2lang(prefix))
    inpdata <- newdata
    evalfun <- calcParNewdata
  }

  # check this for accuracy
  inpdata <- nimble:::convertFactorConstantsToNumeric(inpdata)

  out <- pbapply::pblapply(1:nsamp, function(i, inpdata){
    pars <- lapply(simlist, function(x){
      if(is.null(dim(x))){
        return(x[i])
      }
      inds <- sapply(dim(x), function(y) 1:y)
      inds[[1]] <- i
      do.call(`[`, c(list(x), inds))
      #apply(x, -1, `[`, i)
    })
    
    comblist <- c(inpdata, pars)

    evalfun(lp, LHS, comblist)

  }, inpdata=inpdata)

  out <- simplify2array(out)
  if(!is.null(invlink)) out <- invlink(out)
  out
}

sims_list <- function(samples){
  #params <- remove_params(samples, exclude)
  params <- param_names(samples)
  sapply(strip_params(params, unique=TRUE), get_posterior_array, 
                      samples, simplify=FALSE) 
}

#Remove brackets and indices from parameter names in mcmc.list
strip_params <- function(params_raw, unique=FALSE){
  params_strip <- sapply(strsplit(params_raw,'[', fixed=T),'[',1)
  if(unique) return( unique(params_strip) )
  params_strip
}

#Extract the posterior of a parameter and organize it into an array
get_posterior_array <- function(parameter, samples){
  
  tryCatch({
    #Subset output columns matching parameter
    col_inds <- which_params(parameter, param_names(samples))
    posterior_raw <- do.call(rbind, select_cols(samples, col_inds))
  
    #If parameter is scalar, return it now
    if( ncol(posterior_raw) == 1 ){ return(as.vector(posterior_raw)) }

    #If parameter is array, get indices
    ind_raw <- get_inds(parameter, colnames(posterior_raw))
    ndraws <- nrow(posterior_raw)
    ind_array <- cbind(1:ndraws, ind_raw[rep(1:nrow(ind_raw), each=ndraws),])

    #Create, fill, return output object
    fill_array(as.vector(posterior_raw), ind_array)
  }, error = function(e) {
    message(paste0("Caught error when creating sims.list array for '",
                   parameter,"':\n",e,"\n"))
    NA
  })
}

#Fill an array from vector using matching array indices
fill_array <- function(data_vector, indices){
  out <- array(NA, dim=apply(indices,2,max))
  out[indices] <- data_vector
  out
}

#Extract index values inside brackets from a non-scalar parameter
#param is the "base" name of the parameter and params_raw is a vector of 
#strings that contain brackets
get_inds <- function(param, params_raw){
  inds_raw <- sub(paste(param,'[',sep=''),'', params_raw,fixed=T)
  inds_raw <- sub(']','', inds_raw, fixed=T)
  inds_raw <- strsplit(inds_raw,',',fixed=T)
  inds <- as.integer(unlist(inds_raw))
  matrix(inds, byrow=T, ncol=length(inds_raw[[1]]))
}

#Identify which columns in mcmc.list object correspond to a given
#parameter name (useful for non-scalar parameters)
which_params <- function(param, params_raw){
  params_strip <- strip_params(params_raw)
  if( ! param %in% params_strip ){
    return(NULL)
  } 
  which(params_strip == param)
}

#Subset cols of mcmc.list (simple version of [.mcmc.list method)
select_cols <- function(mcmc_list, col_inds){
  out <- lapply(1:length(mcmc_list), FUN=function(x){
            mcmc_element <- mcmc_list[[x]][,col_inds,drop=FALSE]
            attr(mcmc_element,'mcpar') <- attr(mcmc_list[[x]], 'mcpar')
            class(mcmc_element) <- 'mcmc'
            mcmc_element
            })
  class(out) <- 'mcmc.list'
  out
}

#Get names of parameters from an mcmc.list
#If simplify=T, also drop brackets/indices
param_names <- function(mcmc_list, simplify=FALSE){
  raw <- colnames(mcmc_list[[1]])
  if(!simplify) return(raw)
  strip_params(raw, unique=T)
}
