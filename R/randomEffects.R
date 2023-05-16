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
barToTerms <- function(barExp){
  stopifnot(isBar(barExp))

  # Get random factor
  rfact <- getRandomFactorName(barExp)
  
  # Expand LHS formula into full set of terms
  LHS <- as.formula(as.call(list(as.name("~"), barExp[[2]])))
  trms <- attr(terms(LHS), "term.labels")
  int <- as.logical(attr(terms(LHS), "intercept"))

  # If only intercept on LHS return just factor
  if(length(trms)==0 & int) return(rfact)
  
  all_terms <- list()

  # If intercept, add random factor first
  if(int) all_terms <- c(all_terms, rfact)
  
  # Interact other terms with random factor and add to output list
  trms <- sapply(trms, str2lang)
  all_terms <- c(all_terms, lapply(trms, function(x){
    substitute(PAR:RFACT, list(PAR=x, RFACT=rfact))
  }))
  all_terms
}

#  Get factor name on RHS of bar
getRandomFactorName <- function(barExp){
  barExp[[3]]
}

# Convert bar expression into a complete formula component
# with interactions between terms on LHS of bar and random factor
getCombinedFormulaFromBar <- function(barExp){
  stopifnot(isBar(barExp))
  trms <- barToTerms(barExp)
  addFormulaTerms(trms)
}

# Takes a list of terms and "adds" them together (by inserting +)
# Unfortunately converts code --> string --> code
# Otherwise very hard to get things in correct order without parantheses
addFormulaTerms <- function(trms){

  # If only one term, return it
  if(length(trms) == 1){
    if(is.list(trms)) trms <- trms[[1]]
    return(trms)
  }  
  # Otherwise add them together
  form <- str2lang(paste(sapply(trms, deparse), collapse="+"))
  #form <- substitute(A+B, list(A=trms[[1]], B=trms[[2]]))
  #if(length(trms) > 2){
  #  for (i in 3:length(trms)){
  #    form <- substitute(A+B, list(A=form, B=trms[[i]]))
  #  }
  #}
  form
}

# Get list of names for hyperpriors (SDs) from combined terms
# e.g. term x.group --> sd.x.group
getHyperpriorNames <- function(barExp, prefix){
  stopifnot(isBar(barExp))
  trms <- barToTerms(barExp)
  sdPrefix <- ifelse(is.null(prefix), "", deparse(prefix))
  sd_names <- paste0(prefix, "sd_", sapply(trms, deparse))
  sd_names <- gsub(":", "_", sd_names) # replace : since it can't be in BUGS
  sapply(sd_names, str2lang)
}

# Make hyperprior BUGS code chunk from a bar expression
# (1|group) + dunif(0, 100) --> sd.group ~ dunif(0, 100) 
makeHyperpriorCode <- function(barExp, sdPrefix, sdPrior){
  stopifnot(isBar(barExp))
  sd_names <- getHyperpriorNames(barExp, sdPrefix)

  hyperpriors <- lapply(sd_names, function(x){
    substitute(LHS ~ PRIOR, list(LHS=x, PRIOR=sdPrior))
  })
  embedLinesInCurlyBrackets(hyperpriors)
}

# Generate names for random terms from bar expression and prefix
# (x||group) + beta_ --> beta_group, beta_x_group
makeRandomParNames <- function(barExp, prefix){
  stopifnot(isBar(barExp))
  trms <- barToTerms(barExp)
  par_names <- paste0(deparse(prefix), sapply(trms, deparse))
  par_names <- gsub(":", "_", par_names) # for BUGS compatibility
  sapply(par_names, str2lang)
}

# Get number of levels for factor in a bar expression from constants
numRandomFactorLevels <- function(barExp, constants){
  rfact <- getRandomFactorName(barExp)
  facdata <- constants[[deparse(rfact)]]
  stopifnot(is.factor(facdata))
  as.numeric(length(levels(facdata)))
}

# Make uncorrelated random effects prior(s) from a particular bar expression
makeUncorrelatedRandomPrior <- function(barExp, coefPrefix, sdPrefix, modelInfo){
  nlev <- numRandomFactorLevels(barExp, modelInfo$constants)
  sd_name <- getHyperpriorNames(barExp, sdPrefix)
  stopifnot(length(sd_name) == 1)
  sd_name <- sd_name[[1]]
  par_name <- makeRandomParNames(barExp, coefPrefix)[[1]]
  substitute(LHS[1:NLEV] ~ forLoop(dnorm(0, sd=SD)),
    list(LHS=par_name, NLEV=nlev, SD=sd_name))
}

# Make correlated random effects priors from a particular bar expression
makeCorrelatedRandomPrior <- function(barExp, coefPrefix, sdPrefix, modelInfo){

  stopifnot(isBar(barExp))
  trms <- barToTerms(barExp)  
  np <- as.numeric(length(trms))
  stopifnot(np > 1) # make sure we have at least 2 terms

  # BUGS code to assign hyperprior SDs into vector
  rfact <- getRandomFactorName(barExp)
  sd_names <- getHyperpriorNames(barExp, sdPrefix)
  sdPrefix <- ifelse(is.null(sdPrefix), "", deparse(sdPrefix))
  sd_vec <- as.name(paste0(sdPrefix, "re_sds_", deparse(rfact)))
  sds <- lapply(1:length(sd_names), function(i){
    substitute(SDS[IDX] <- SDPAR, 
               list(SDS = sd_vec, IDX=as.numeric(i), SDPAR=sd_names[[i]]))
  })
  sds <- embedLinesInCurlyBrackets(sds)

  # BUGS code for Ustar and U
  Ustar_name <- as.name(paste0("Ustar_",deparse(rfact)))
  U_name <- as.name(paste0("U_",deparse(rfact)))
  u <- substitute({
    USTAR[1:NP, 1:NP] ~ dlkj_corr_cholesky(1.3, NP)
    U[1:NP, 1:NP] <- uppertri_mult_diag(USTAR[1:NP, 1:NP], SDS[1:NP])
    }, list(USTAR=Ustar_name, U=U_name, NP=np, SDS=sd_vec)
  )

  # Generate name of random effects mean vector (of all 0s)
  rfact <- getRandomFactorName(barExp)
  re_means <- as.name(paste0("re_means_", deparse(rfact)))
  re_mean_loop <- substitute(REMEANS[1:NP] <- rep(0, NP), 
                             list(REMEANS=re_means, NP=np))
  # Get index
  if(!is.null(modelInfo$indexCreator)){
    idx <- as.name(modelInfo$indexCreator())
  } else{
    idx <- quote(i_) # if no indexCreator is available, use a placeholder
  }

  # Generate BUGS code for B
  B_name <- as.name(paste0("B_",deparse(rfact)))
  B <- substitute(B[IDX, 1:NP] ~ dmnorm(REMEANS[1:NP], cholesky = U[1:NP, 1:NP], prec_param=0),
                  list(B=B_name, IDX=idx, NP=np, REMEANS=re_means, U=U_name))
  # Generate BUGS code to split parts of B out into separate vectors for each parameter
  par_names <- makeRandomParNames(barExp, coefPrefix)
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
makeRandomPriorCode <- function(barExp, coefPrefix, sdPrefix, modelInfo){
  stopifnot(isBar(barExp))
  trms <- barToTerms(barExp)
  if(length(trms) == 1){
    return(makeUncorrelatedRandomPrior(barExp, coefPrefix, sdPrefix, modelInfo))
  }
  makeCorrelatedRandomPrior(barExp, coefPrefix, sdPrefix, modelInfo)
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
  stopifnot(isBar(barExp))
  RHS <- barExp[[3]]
  is_nested <- is.call(RHS) && RHS[[1]] == ":"
  # If no nesting return inputs
  if(!is_nested) return(list(barExp=barExp, constants=constants))

  # Create new combined random factor term
  fac_names <- strsplit(deparse(RHS), ":")[[1]]
  comb_name <- paste(fac_names, collapse="_")
  barExp[[3]] <- as.name(comb_name)

  # Generate new combined random factor and add it to constants
  # Only make new factor if it hasn't already been done
  if(! comb_name %in% names(constants)){
    fac_dat <- constants[fac_names]
    fac_len <- sapply(fac_dat, length)
    stopifnot(all(sapply(fac_dat, class) == "factor"))
    stopifnot(all(fac_len == fac_len[1]))
    new_fac <- apply(as.data.frame(fac_dat), 1, paste, collapse=":")
    new_fac <- factor(new_fac)
    constants[[comb_name]] <- new_fac
  }

  list(barExp=barExp, constants=constants)
}

# Function to process a single bar expression (barExp) such as (1|group)
# SDprior is the desired hyperprior, prefix is the prefix on the parameters,
# and constants are passed so they can be modified if needed
processBar <- function(barExp, sdPrior, coefPrefix, sdPrefix, modelInfo){  
  # Handle nested random effects
  nested <- processNestedRandomEffects(barExp, modelInfo$constants)
  barExp <- nested$barExp
  modelInfo$constants <- nested$constants
  # Get random factor name
  rfact <- getRandomFactorName(barExp)
  # Get new formula component
  form <- getCombinedFormulaFromBar(barExp)
   
  # BUGS Hyperprior code
  hyperpriors <- makeHyperpriorCode(barExp, sdPrefix, sdPrior)
  # BUGS random effect prior code, also updates constants if needed
  priors <- makeRandomPriorCode(barExp, coefPrefix, sdPrefix, modelInfo)
  # Combine all code
  code <- embedLinesInCurlyBrackets(list(hyperpriors, priors))
  # Return formula component, prior code, and (possibly) updated model info
  list(formula=form, code = removeExtraBrackets(code), modelInfo=modelInfo)
}


# Function to handle all bar expressions in a formula, combining results

#' @importFrom lme4 findbars
processAllBars <- function(formula, sdPrior, coefPrefix, sdPrefix, modelInfo){
  # Generate separate bars from formula
  bars <- lme4::findbars(formula)

  # Return NULL if there are no bars
  if(is.null(bars)) return(list(formula=NULL, code=NULL, modelInfo=modelInfo))

  # Create empty output list
  out <- vector("list", length(bars))
  names(out) <- sapply(bars, deparse)

  # Fill in first element of list with first bar expression
  out[[1]] <- processBar(bars[[1]], sdPrior, coefPrefix, sdPrefix, modelInfo)
  # Work through remaining bar expressions if they exist
  # Make sure to pass updated model info
  if(length(bars) > 1){
    for (i in 2:length(bars)){
      out[[i]] <- processBar(bars[[i]], sdPrior, coefPrefix, sdPrefix, 
                             out[[i-1]]$modelInfo)
    }
  }
  
  # Combine formula terms from all bar expressions
  full_formula <- unlist(lapply(out, function(x) x$formula))
  dups <- names(full_formula)[duplicated(full_formula)]
  if(length(dups) > 0){
    dups <- paste0("(",dups,")")
    stop("Term(s) ", paste(dups, collapse=", "), " are duplicated in formula", call.=FALSE)
  }
  full_formula <- addFormulaTerms(full_formula)
  
  # Combine all BUGS priors code
  full_priors <- lapply(out, function(x) x$code)
  full_priors <- embedLinesInCurlyBrackets(full_priors)
  full_priors <- removeExtraBrackets(full_priors) # clean up
  
  # Return formula, BUGS priors code, and final updated modelInfo
  list(formula = full_formula, code=full_priors, modelInfo=out[[length(out)]]$modelInfo)
}
