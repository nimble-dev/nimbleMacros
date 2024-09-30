# Check if input is a proper bar expression
isBar <- function(code){
  if(!is.call(code)) return(FALSE)
  if(length(code) != 3) return(FALSE)
  if(code[[1]] != "|") return(FALSE)
  TRUE
}

# Convert bar expression to a list of interaction terms combining 
# the LHS and RHS of bar
# Also first expand LHS if needed (e.g. from x*y to x+y+x:y)
#' @importFrom stats as.formula terms
barToTerms <- function(barExp, keep_idx = FALSE, formula_info = NULL){
  if(!isBar(barExp)) stop("Input is not bar expression")

  # Get random factor
  rfact <- safeDeparse(getRandomFactorName(barExp, keep_idx))
  
  # Expand LHS formula into full set of terms
  LHS <- as.formula(as.call(list(as.name("~"), barExp[[2]])))
  if(!keep_idx) LHS <- removeBracketsFromFormula(LHS)
  trms <- attr(terms(LHS), "term.labels")
  int <- as.logical(attr(terms(LHS), "intercept"))

  # If only intercept on LHS return just factor
  if(length(trms)==0 & int) return(rfact)
  
  all_terms <- character(0)

  # If intercept, add random factor first
  if(int) all_terms <- c(all_terms, rfact)
  
  # Interact other terms with random factor and add to output list
  all_terms <- c(all_terms, sapply(trms, function(x){
    paste0(x,":",rfact)
  }))
  all_terms <- unname(all_terms)

  if(!is.null(formula_info)){
    # Update order of interactions to match the final, combined formula
    all_terms <- fixTerms(all_terms, formula_info)
  }

  all_terms
}

#  Get factor name on RHS of bar
getRandomFactorName <- function(barExp, keep_idx = FALSE){
  out <- barExp[[3]]
  if(is.name(out)) return(out)
  if(out[[1]] == "["){
    if(keep_idx){
      return(out)
    } else{
      return(out[[2]])
    }
  }
  stop("Something went wrong")
}

# Get list of names for hyperpriors (SDs) from combined terms
# e.g. term x.group --> sd.x.group
getHyperpriorNames <- function(barExp, modelInfo, formula_info, prefix){
  if(!isBar(barExp)) stop("Input is not bar expression")  
  sd_prefix <- ifelse(is.null(prefix), "", safeDeparse(prefix))

  trms <- barToTerms(barExp, formula_info = formula_info)

  rfact <- safeDeparse(getRandomFactorName(barExp))
  
  out <- lapply(trms, function(x){

    form <- as.formula(str2lang(paste0("~0+", x)))
    par_dim <- makeEmptyParameterStructure(form, modelInfo$constants)[[1]]

    # Check if intercept
    is_int <- length(dim(par_dim)) == 1

    # Check if no factors in random slopes
    cov_ind <- which(strsplit(x, ":")[[1]] != rfact)
    no_factor <- length(dim(par_dim)) == 2 && dim(par_dim)[cov_ind] == 1

    if(is_int){
      sd_names <- list(str2lang(paste0(sd_prefix, "sd_", rfact)))
    } else if(no_factor){
      sd_names <- list(str2lang(paste0(sd_prefix, "sd_", gsub(":", "_", x))))
    } else { # at least one component of slope is factor
      dim_short_names <- strsplit(x, ":")[[1]]
      rfact_ind <- which(dim_short_names == rfact)
      nms <- attributes(par_dim)$dimnames
      nms[rfact_ind] <- rfact
      nms <- expand.grid(nms)
      nms <- apply(nms, 1, function(x) paste(as.character(x), collapse="_"))
      sd_names <- paste0(sd_prefix, "sd_", nms) 
      sd_names <- lapply(sd_names, str2lang)
    } 
    #if(length(sd_names) == 1) sd_names <- sd_names[[1]]
    sd_names
  })
  out
}

# Make hyperprior BUGS code chunk from a bar expression
# (1|group) + dunif(0, 100) --> sd.group ~ dunif(0, 100) 
makeHyperpriorCode <- function(barExp, modelInfo, formula_info, sdPrefix, priorSpecs){
  if(!isBar(barExp)) stop("Input is not bar expression")
  sd_names <- getHyperpriorNames(barExp, modelInfo, formula_info, sdPrefix)

  hyperpriors <- lapply(sd_names, function(x){
    lapply(x, function(z){
      # If no prior settings were provided the output of this function is probably
      # not going to be used later, so just insert a placeholder
      if(is.null(priorSpecs)){
        sdPrior <- quote(PLACEHOLDER)
      } else {
        sdPrior <- matchPrior(z, "sd", priorSpecs=priorSpecs)
      }
      substitute(LHS ~ PRIOR, list(LHS=z, PRIOR=sdPrior))
    })
  })
  embedLinesInCurlyBrackets(unlist(hyperpriors))
}

# Generate names for random terms from bar expression and prefix
# (x||group) + beta_ --> beta_group, beta_x_group
makeRandomParNames <- function(barExp, prefix, formula_info){
  if(!isBar(barExp)) stop("Input is not bar expression")
  trms <- barToTerms(barExp, formula_info=formula_info)
  par_names <- paste0(safeDeparse(prefix), trms)
  #par_names <- gsub(":", "_", par_names) # for BUGS compatibility
  par_names <- gsub(":(?=(((?!\\]).)*\\[)|[^\\[\\]]*$)", "_", par_names, perl=TRUE) # for BUGS compatibility
  sapply(par_names, str2lang)
}

# Get number of levels for factor in a bar expression from constants
numRandomFactorLevels <- function(barExp, constants){
  rfact <- getRandomFactorName(barExp)
  facdata <- constants[[safeDeparse(rfact)]]
  if(!(is.factor(facdata)|is.character(facdata))) stop("Grouping cov is not a factor")
  if(is.character(facdata)) facdata <- as.factor(facdata)
  as.numeric(length(levels(facdata)))
}

# Make uncorrelated random effects prior(s) from a particular bar expression
makeUncorrelatedRandomPrior <- function(barExp, coefPrefix, sdPrefix, modelInfo, 
                                        formula_info, noncenter=FALSE, centerVar=NULL){
  nlev <- numRandomFactorLevels(barExp, modelInfo$constants)
  sd_names <- getHyperpriorNames(barExp, modelInfo, formula_info, sdPrefix)
  if(length(sd_names) > 1) stop("sd_names should be length 1 here", call.=FALSE)
  sd_names <- sd_names[[1]]

  par_name <- makeRandomParNames(barExp, coefPrefix, formula_info=formula_info)[[1]]
  rand_mean <- getUncorrelatedRandomEffectMean(barExp, coefPrefix, modelInfo, centerVar)

  sd_prefix <- ifelse(is.null(sdPrefix), "", safeDeparse(sdPrefix))
  trm <- barToTerms(barExp, formula_info=formula_info)
  form <- as.formula(str2lang(paste0("~0+", trm)))
  par_dim <- makeEmptyParameterStructure(form, modelInfo$constants)[[1]]
  trm_split <- strsplit(trm, ":")[[1]] 
  trm_split <- trm_split[attributes(par_dim)$dim > 1] 
  par_dim <- drop(par_dim)
  rfact <- safeDeparse(getRandomFactorName(barExp))
  rfact_ind <- which(trm_split== rfact)
 
  if(length(dim(par_dim)) > 1){
    pd <- dim(par_dim)
    #pd <- pd[1:(length(pd)-1)]
    pd <- lapply(pd, function(x) 1:x)
    pd[rfact_ind] <- paste0("1:",nlev)
    pd <- expand.grid(pd)
    pd <- apply(pd, 1, function(x) paste(x, collapse=","))
    idx <- sapply(pd, function(x) paste0("[", x, "]"))
  } else {
    idx <- paste0("[1:",nlev,"]")
  }
  r_lhs <- paste0(safeDeparse(par_name), idx)
  r_lhs_raw <- paste0(safeDeparse(par_name), "_raw", idx)
  r_lhs <- lapply(r_lhs, str2lang)
  r_lhs_raw <- lapply(r_lhs_raw, str2lang)

  out <- lapply(1:length(sd_names), function(i){

    if(noncenter){     
      substitute({
        LHS_RAW ~ nimbleMacros::FORLOOP(dnorm(0, sd=1))
        LHS <- nimbleMacros::FORLOOP(MEAN + SD * LHS_RAW)
        }, 
        list(LHS=r_lhs[[i]], LHS_RAW = r_lhs_raw[[i]],
             MEAN=rand_mean, SD=sd_names[[i]]))
    } else {
      substitute(LHS ~ nimbleMacros::FORLOOP(dnorm(MEAN, sd=SD)),
                 list(LHS=r_lhs[[i]], MEAN=rand_mean, SD=sd_names[[i]]))
    }
  })
  embedLinesInCurlyBrackets(out)
}

# Figure out of mean of random effects should be 0 (non-centered)
# or centered on some mean value based on which grouping factor we're dealing with
getUncorrelatedRandomEffectMean <- function(barExp, coefPrefix, modelInfo, centerVar){
  if(is.null(centerVar)) return(0)

  rfact <- getRandomFactorName(barExp)
  if(identical(rfact, centerVar)){
    bar_lhs <- barExp[[2]]
    bar_lhs <- list(as.name("~"), bar_lhs)
    bar_lhs <- as.call(bar_lhs)
    bar_lhs <- as.formula(bar_lhs)
    dat <- makeDummyDataFrame(bar_lhs, modelInfo$constants)
    bar_lhs <- removeBracketsFromFormula(bar_lhs)
    par_struct <- makeParameterStructure(bar_lhs, dat)
    par_name <- getParametersForLP(names(par_struct), coefPrefix)
    out <- str2lang(par_name)
  } else {
    out <- 0
  }
  out
}

barExpHasFactor <- function(barExp, data){
  if(!isBar(barExp)) stop("Input is not bar expression")
  lhs <- barExp[[2]]
  form <- as.formula(as.call(list(as.name("~"), lhs)))
  vars <- all.vars(form)
  types <- sapply(vars, function(x) class(data[[x]]))
  any(types == "factor")

}

# Make correlated random effects priors from a particular bar expression
makeCorrelatedRandomPrior <- function(barExp, coefPrefix, sdPrefix, modelInfo,
                                      formula_info, centerVar=NULL, priorInfo){

  if(!isBar(barExp)) stop("Input is not bar expression")
  trms <- barToTerms(barExp)  
  np <- as.numeric(length(trms))
  if(np < 2) stop("Need at least 2 terms")

  if(barExpHasFactor(barExp, modelInfo$constants)){
    stop("Correlated random slopes for factors not yet supported.\nTry converting to dummy variables instead.", call.=FALSE)
  }

  # BUGS code to assign hyperprior SDs into vector
  rfact <- getRandomFactorName(barExp)
  sd_names <- getHyperpriorNames(barExp, modelInfo, formula_info, sdPrefix)
  sdPrefix <- ifelse(is.null(sdPrefix), "", safeDeparse(sdPrefix))
  sd_vec <- as.name(paste0(sdPrefix, "re_sds_", safeDeparse(rfact)))

  #temporary until factors supported
  sd_names <- unlist(sd_names)

  sds <- lapply(1:length(sd_names), function(i){
    substitute(SDS[IDX] <- SDPAR, 
               list(SDS = sd_vec, IDX=as.numeric(i), SDPAR=sd_names[[i]]))
  })
  sds <- embedLinesInCurlyBrackets(sds)

  # BUGS code for Ustar and U
  Ustar_name <- as.name(paste0("Ustar_",safeDeparse(rfact)))
  U_name <- as.name(paste0("U_",safeDeparse(rfact)))
  # LKJ distribution shape parameter
  eta <- priorInfo$eta
  if(is.null(eta)) eta <- 1.3
  u <- substitute({
    USTAR[1:NP, 1:NP] ~ dlkj_corr_cholesky(ETA, NP)
    U[1:NP, 1:NP] <- uppertri_mult_diag(USTAR[1:NP, 1:NP], SDS[1:NP])
    }, list(USTAR=Ustar_name, U=U_name, NP=np, SDS=sd_vec, ETA=eta)
  )

  # Generate name of random effects mean vector
  # This will be all 0s if center = NULL, or center does not match the bar expression
  mean_info <- getCorrelatedRandomEffectMeanCode(barExp, coefPrefix, modelInfo, centerVar)
  re_means <- mean_info$re_means
  re_mean_loop <- mean_info$re_mean_loop

  # Get index
  if(!is.null(modelInfo$indexCreator)){
    idx <- as.name(modelInfo$indexCreator())
  } else{
    idx <- quote(i_) # if no indexCreator is available, use a placeholder
  }

  # Generate BUGS code for B
  B_name <- as.name(paste0("B_",safeDeparse(rfact)))
  B <- substitute(B[IDX, 1:NP] ~ dmnorm(REMEANS[1:NP], cholesky = U[1:NP, 1:NP], prec_param=0),
                  list(B=B_name, IDX=idx, NP=np, REMEANS=re_means, U=U_name))
  # Generate BUGS code to split parts of B out into separate vectors for each parameter
  par_names <- makeRandomParNames(barExp, coefPrefix, formula_info = formula_info)
  B_split <- lapply(1:np, function(j){
    substitute(PAR[IDX] <- B[IDX, J], 
               list(PAR=par_names[[j]], IDX=idx, B=B_name, J=as.numeric(j)))
  })

  # Get number of factor levels for random factor
  nlev <- numRandomFactorLevels(barExp, modelInfo$constants)

  # Generate BUGS code combining B and B_split into a for loop
  B_loop <- embedLinesInCurlyBrackets(c(B, B_split))
  B_loop <- c(list(as.name("for"), idx, substitute(1:NLEV, list(NLEV=nlev))),
              B_loop)
  B_loop <- as.call(B_loop)

  # Return BUGS code combining all parts
  embedLinesInCurlyBrackets(list(sds, u, re_mean_loop, B_loop))
}

# Function to generate the correct code for the mean vector, depending on the
# grouping factor, bar expression, and if the random effect should be 'centered'
getCorrelatedRandomEffectMeanCode <- function(barExp, coefPrefix, modelInfo, centerVar){

  rfact <- getRandomFactorName(barExp)
  trms <- barToTerms(barExp)  
  np <- as.numeric(length(trms))
  re_means = as.name(paste0("re_means_", safeDeparse(rfact)))

  if(is.null(centerVar) | !identical(rfact, centerVar)){
    re_mean_loop <- substitute(REMEANS[1:NP] <- rep(0, NP), 
                             list(REMEANS=re_means, NP=np))
  } else {
    bar_lhs <- barExp[[2]]
    bar_lhs <- list(as.name("~"), bar_lhs)
    bar_lhs <- as.call(bar_lhs)
    bar_lhs <- as.formula(bar_lhs)
    dat <- makeDummyDataFrame(bar_lhs, modelInfo$constants)
    bar_lhs <- removeBracketsFromFormula(bar_lhs)
    par_struct <- makeParameterStructure(bar_lhs, dat)
    par_names <- getParametersForLP(names(par_struct), coefPrefix)
    par_names <- lapply(par_names, str2lang)

    re_mean_loop <- lapply(1:length(par_names), function(i){
      substitute(MNS[IDX] <- MNPAR, 
                list(MNS = re_means, IDX=as.numeric(i), MNPAR=par_names[[i]]))
    })
    re_mean_loop <- embedLinesInCurlyBrackets(re_mean_loop)
  }
  list(re_means = re_means, re_mean_loop = re_mean_loop)
}

# Nimble function needed above

#' uppertri_mult_diag
#' 
#' nimbleFunction needed when fitting correlated random effects.
#' Generates upper triangular Cholesky factor of covariance matrix (U in code)
#' from upper tri Cholesky factor of correlation matrix (Ustar in code)
#' and vector of standard deviations. Taken from the NIMBLE manual, 
#' section 5.2.4.1.2 LKJ distribution for correlation matrices.
#' 
#' @param mat upper triangular Cholesky factor of correlation matrix (Ustar)
#' @param vec vector of standard deviations for individual random effects
#'
#' @name uppertri_mult_diag

#' @importFrom nimble nimMatrix
#' @export
uppertri_mult_diag <- nimbleFunction(
    run = function(mat = double(2), vec = double(1)) {
        returnType(double(2))
        p <- length(vec)
        out <- matrix(nrow = p, ncol = p, init = FALSE)
        for(i in 1:p)
            out[ , i] <- mat[ , i] * vec[i]
        return(out)
})

# Decide if correlated or uncorrelated random prior(s) are needed from bar expression
# And return corresponding BUGS code and constants
# 'centerVar' can (optionally) be a grouping factor to "centerVar" the random effects on, e.g.
# instead of: alpha + beta * x + re[group], where re[group] ~ dnorm(0, sd_group)
# we have: beta * x + re[group], where re[group] ~ dnorm(alpha, sd_group)
# If NULL, re mean will be 0, or if the grouping factor provided to
# 'centerVar' does not match the one in the bar expression, then re mean will be 0.
makeRandomPriorCode <- function(barExp, coefPrefix, sdPrefix, modelInfo, formula_info, 
                                noncenter = FALSE, centerVar = NULL, priorInfo){
  if(!isBar(barExp)) stop("Input is not bar expression")

  if(!is.null(centerVar)){
    if(barExpHasFactor(barExp, modelInfo$constants)){
      stop("Centered random slopes for factors not yet supported.\nTry converting to dummy variables instead.", call.=FALSE)
    }
  }

  trms <- barToTerms(barExp)
  if(length(trms) == 1){
    return(makeUncorrelatedRandomPrior(barExp, coefPrefix, sdPrefix, modelInfo, 
                                       formula_info, noncenter, centerVar))
  }
  if(noncenter) stop("Uncentered not supported for correlated random effects yet", call.=FALSE)
  makeCorrelatedRandomPrior(barExp, coefPrefix, sdPrefix, modelInfo, 
                            formula_info, centerVar, priorInfo)
}

# Remove extra brackets in BUGS code
removeExtraBrackets <- function(code){
  as.call(removeExtraBracketsInternal(code))
}

removeExtraBracketsInternal <- function(code){
  unlist(lapply(code, function(x){
    if(length(x) == 1) return(x)                       
    if(x[[1]] == "{") x <- as.list(x)[2:length(x)]
    if(is.list(x)){
      x <- removeExtraBracketsInternal(x)
    } else if(x[[1]] == "for"){
      x[[4]] <- removeExtraBrackets(x[[4]])
    }
    x
  }))
}

# Does initial processing on bar expressions with nested random effects
# e.g. (1|group:group2)
# Does two things:
# 1. Converts a RHS term combo like group:group2 into a single term group_group2
#    which allows it to be used in formula and avoids : issues in BUGS
# 2. Actually creates new factor group_group2 in constants, which is a combination
#    of levels of group and group2 (following lme4)
processNestedRandomEffects <- function(barExp, constants){
  if(!isBar(barExp)) stop("Input is not bar expression")
  RHS <- barExp[[3]]
  is_nested <- is.call(RHS) && RHS[[1]] == ":"
  # If no nesting return inputs
  if(!is_nested) return(list(barExp=barExp, constants=constants))

  # Create new combined random factor term
  fac_names <- strsplit(safeDeparse(RHS), ":")[[1]]
  comb_name <- paste(fac_names, collapse="_")
  barExp[[3]] <- as.name(comb_name)

  # Generate new combined random factor and add it to constants
  # Only make new factor if it hasn't already been done
  if(! comb_name %in% names(constants)){
    fac_dat <- constants[fac_names]
    fac_len <- sapply(fac_dat, length)
    if(!all(fac_len == fac_len[1])) stop("All factors should be same length")

    are_facs <- all(sapply(fac_dat, is.factor))
    are_chars <- all(sapply(fac_dat, is.character))
    if(!(are_facs | are_chars)) stop("At least one grouping cov is not factor")

    if(are_facs){
      new_fac <- apply(as.data.frame(fac_dat), 1, paste, collapse=":")
      new_fac <- factor(new_fac)
    } else if(are_chars){
      match_dim <- dim(fac_dat[[1]])
      if(!(all(sapply(lapply(fac_dat, dim), function(x) identical(x, match_dim))))){
        stop("All factor covs should have same dimensions")
      }
      if(is.null(match_dim)){
        new_fac <- apply(as.data.frame(fac_dat), 1, paste, collapse=":")
      } else {
        as_vecs <- as.data.frame(lapply(fac_dat, as.vector))
        new_fac <- apply(as_vecs, 1, paste, collapse=":")
        new_fac <- array(new_fac, dim=match_dim)
      }
    } else {
      stop("Invalid input", call.=FALSE)
    }
    constants[[comb_name]] <- new_fac
  }

  list(barExp=barExp, constants=constants)
}


# Function to process a single bar expression (barExp) such as (1|group)
# SDprior is the desired hyperprior, prefix is the prefix on the parameters,
# and constants are passed so they can be modified if needed
processBar <- function(barExp, priorInfo, coefPrefix, sdPrefix, modelInfo, 
                       formula_info, noncenter = FALSE, centerVar=NULL){  
  # Handle nested random effects
  nested <- processNestedRandomEffects(barExp, modelInfo$constants)
  barExp <- nested$barExp
  modelInfo$constants <- nested$constants
  # Get random factor name
  rfact <- getRandomFactorName(barExp)
  # Get new formula terms (NOT SURE THIS IS NEEDED!!)
  trms <- barToTerms(barExp, keep_idx=TRUE, formula_info = formula_info)
   
  # BUGS Hyperprior code
  hyperpriors <- makeHyperpriorCode(barExp, modelInfo, formula_info, sdPrefix, priorInfo)
  # BUGS random effect prior code, also updates constants if needed
  priors <- makeRandomPriorCode(barExp, coefPrefix, sdPrefix, modelInfo,
                                formula_info, noncenter, centerVar, priorInfo)
  # Combine all code
  code <- embedLinesInCurlyBrackets(list(hyperpriors, priors))
  # Return formula component, prior code, and (possibly) updated model info
  list(terms=trms, code = removeExtraBrackets(code), modelInfo=modelInfo)
}


# Function to handle all bar expressions in a formula, combining results

processAllBars <- function(formula, priors, coefPrefix, sdPrefix, modelInfo, 
                           formula_info, noncenter = FALSE, centerVar=NULL){
  # Generate separate bars from formula
  #formula <- removeBracketsFromFormula(formula) 
  bars <- reformulas::findbars(formula)

  # Return NULL if there are no bars
  if(is.null(bars)) return(list(formula=NULL, code=NULL, modelInfo=modelInfo))

  # Create empty output list
  out <- vector("list", length(bars))
  names(out) <- sapply(bars, safeDeparse)

  # Fill in first element of list with first bar expression
  out[[1]] <- processBar(bars[[1]], priors, coefPrefix, sdPrefix, modelInfo, 
                         formula_info, noncenter, centerVar)
  # Work through remaining bar expressions if they exist
  # Make sure to pass updated model info
  if(length(bars) > 1){
    for (i in 2:length(bars)){
      out[[i]] <- processBar(bars[[i]], priors, coefPrefix, sdPrefix, 
                             out[[i-1]]$modelInfo, formula_info, noncenter, centerVar)
    }
  }
  
  # Combine terms from all bar expressions
  full_terms <- unlist(lapply(out, function(x) x$terms), use.names=FALSE)
  dups <- full_terms[duplicated(full_terms)]
  if(length(dups) > 0){
    stop("Term(s) ", paste(dups, collapse=", "), " are duplicated in formula", call.=FALSE)
  }
  
  # Combine all BUGS priors code
  full_priors <- lapply(out, function(x) x$code)
  full_priors <- embedLinesInCurlyBrackets(full_priors)
  full_priors <- removeExtraBrackets(full_priors) # clean up
  
  # Return formula, BUGS priors code, and final updated modelInfo
  list(terms = full_terms, code=full_priors, modelInfo=out[[length(out)]]$modelInfo)
}
