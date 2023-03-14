makeParSkeleton <- function(formula, data){
  pars <- all.vars(formula)
  trm <- attr(terms(formula), "term.labels")
  if(attr(terms(formula), "intercept")) trm <- c("Intercept", trm)
  comb <- strsplit(trm, ":")
  
  mm <- colnames(model.matrix(formula, data))
  
  levs <- sapply(pars, function(x){
    if(!is.factor(data[[x]])) return(NA)
    levels(data[[x]])
  })
  levs_comb <- lapply(1:length(levs), function(i) paste0(names(levs)[i], na.omit(levs[[i]])))
  names(levs_comb) <- names(levs)

  par_skel <- lapply(comb, function(x){
    dm <- sapply(x, function(y) length(levs[[y]]))
    dnames <- lapply(x, function(y) levs_comb[[y]])
    array(0, dim=as.numeric(dm), dimnames=dnames)
  })
  names(par_skel) <- trm

  pars_full <- strsplit(mm, ":")

  par_skel <- lapply(par_skel, function(x){
    for (i in 1:length(pars_full)){
      ind <- t(pars_full[[i]])
      if(length(dim(x)) == length(ind) & all(ind %in% unlist(dimnames(x)))){
        x[ind] <- 1
      }
    }
    drop(x)
  })

  par_skel


}

makeLP <- function(formula, data, idx, prefix){
  formula_noidx <- as.formula(gsub("\\[.*?\\]", "", deparse(formula)))

  # Get original indices
  inds <- extractAllBrackets(formula)
  if(is.null(inds)){
    idx <- paste0("[",deparse(idx),"]")
    inds <- replicate(idx, n=length(all.vars(formula_noidx)))
    names(inds) <- all.vars(formula_noidx)
  }
  inds <- removeDuplicateIndices(inds)

  #idx <- as.character(deparse(idx))
  pars <- all.vars(formula_noidx)
  par_skel <- makeParSkeleton(formula_noidx, data)
  is_factor <- sapply(pars, function(x) is.factor(data[[x]]))
  type <- strsplit(names(par_skel), ":")
  fc <- sapply(type, function(x) x[x %in% pars[is_factor]])
  cont <- sapply(type, function(x) x[! x %in% pars[is_factor] & x != "Intercept"])
  
  par_names <- getParametersForLP(names(par_skel), prefix)
  lp_comps <- lapply(1:length(par_names), function(i){
    out <- paste(par_names[i])
  
    fc_idx <- ''
    if(length(fc[[i]]) > 0){
      #fc_idx <- paste(paste0(fc[[i]],"_NEW[",idx,"]"), collapse = ", ")
      if(!fc[[i]] %in% names(inds)){
        stop("Missing bracket index on RHS of formula", call.=FALSE)
      }
      idx_match <- inds[[fc[[i]]]]
      fc_idx <- paste(paste0(fc[[i]],"_NEW",idx_match), collapse = ", ")
      out <- paste0(out, "[",fc_idx,"]")
    }

    if(length(cont[[i]]) > 0){
      if(!cont[[i]] %in% names(inds)){
        stop("Missing bracket index on RHS of formula", call.=FALSE)
      }
      idx_match <- inds[[cont[[i]]]]
      out <- paste(out, "*", paste0(cont[[i]], idx_match, collapse=" * "))
      #out <- paste(out, "*", paste0(cont[[i]], "[", idx, "]", collapse=" * "))
    }
    out
  })
  names(lp_comps) <- par_names

  lp <- str2lang(paste(unlist(lp_comps), collapse=" + "))
  lp
}


makePriors <- function(formula, data, prior, prefix){ 

  par_skel <- makeParSkeleton(formula, data)
  par_names <- getParametersForLP(names(par_skel), prefix)
  #prior <- quote(dnorm(0, sd=100))
  all_priors <- lapply(1:length(par_skel), function(i){
  
    if(length(par_skel[[i]]) < 2){
      return(substitute(LHS ~ PRIOR, list(LHS=str2lang(par_names[i]), PRIOR=prior)))
    }

    fixed_inds <- which(par_skel[[i]] == 0, arr.ind=TRUE)
    if(length(fixed_inds) == 1) fixed_inds <- as.matrix(fixed_inds)
    fixed_assign <- list()
    if(length(fixed_inds) > 0){
      fixed_assign <- lapply(1:nrow(fixed_inds), function(j){
        inds <- paste0("[",paste(fixed_inds[j,], collapse=","),"]")
        out <- str2lang(paste0(par_names[i], inds))
        substitute(LHS <- 0, list(LHS=out))
      }
      )
    }
    est_inds <- which(par_skel[[i]] == 1, arr.ind=TRUE)
    if(!is.matrix(est_inds)) est_inds <- as.matrix(est_inds)
    est_prior <- lapply(1:nrow(est_inds), function(j){
      inds <- paste0("[",paste(est_inds[j,], collapse=","),"]")
      out <- str2lang(paste0(par_names[i], inds))
      substitute(LHS ~ PRIOR, list(LHS=out, PRIOR=prior))
    }
    )
    both <- c(fixed_assign, est_prior)
    both
  })
  all_priors <- unlist(all_priors)
  embedLinesInCurlyBrackets(all_priors)
}

#' @export
linPred2 <- list(
  process = function(code, .constants, .env){
    RHS <- getRHS(code)
    prefix <- RHS$prefix
    if(is.null(prefix)){
      prefix <- quote(beta_)
    } else {
      RHS$prefix <- NULL 
    }
    RHS <- RHS[[2]]
    if(RHS[[1]] != quote(`~`)) RHS <- c(quote(`~`),RHS) # 
    form <- as.formula(RHS)
    LHS_ind <- extractIndices(getLHS(code))[[1]]

    newConstants <- .constants
    for (i in 1:length(.constants)){
      if(is.factor(.constants[[i]])){
        newConstants[[paste0(names(.constants)[i], "_NEW")]] <- as.numeric(.constants[[i]])
      }
    }

    form_noidx <- as.formula(gsub("\\[.*?\\]", "", deparse(form)))
    dat <- as.data.frame(.constants[all.vars(form_noidx)])

    out <- makeLP(form, dat, LHS_ind, prefix)
    out <- as.call(list(quote(forLoop), out))

    RHS(code) <- out
    list(code=code, constants=newConstants)
  }
)
class(linPred2) <- "model_macro"


#' @export
priors2 <- list(process=function(code, .constants, .env=env){
  form <- nimbleMacros:::getRHS(code)[1:2][[2]]
  if(form[[1]] != quote(`~`)) form <- c(quote(`~`),form) # 
  form <- as.formula(form)
  dat <- as.data.frame(.constants[all.vars(form)])

  prefix <-nimbleMacros:::getLHS(code)
  priors <- nimbleMacros:::getRHS(code)[[3]]

  out <- makePriors(form, dat, priors, prefix=as.character(deparse(prefix)))
  
  list(code=out, constants=.constants)
})
class(priors2) <- "model_macro"

