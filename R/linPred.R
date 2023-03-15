
# Generate separate dummy variables from factors in constants
splitFactorsInConstants <- function(constants){

  is_factor <- sapply(constants, is.factor)
  factor_names <- names(constants)[is_factor]
  for (i in factor_names){
    mm <- model.matrix(formula(paste("~",i,"-1")), as.data.frame(constants))
    mm <- as.list(as.data.frame(mm))
    constants <- c(constants, mm)
  }
  constants
}

# Fill in any data not in constants (assumed to be numeric)
# and generate a model matrix
# Just want the colnames from this so the actual data don't matter
modelMatrixCols <- function(form, constants){
  # Remove index brackets from formula
  form <- as.formula(gsub("\\[.*?\\]", "", deparse(form)))
  nm <- all.vars(form)
  in_data <- names(constants)
  data <- as.data.frame(constants[nm[nm %in% in_data]])
  n <- nrow(data)
  for (i in nm){
    if(is.null(data[[i]])) data[[i]] <- rep(0, n)
  }
  colnames(model.matrix(form, data))
}

# Get parameter names by adding prefix
getParametersForLP <- function(components, prefix="beta_"){
  components <- gsub("(","", components, fixed=TRUE)
  components <- gsub(")", "", components, fixed=TRUE)
  components <- gsub(":", ".", components, fixed=TRUE)
  paste0(prefix, components)
  #paste0("beta[",1:length(components),"]")
}

# Extract entire bracket structure
extractBracket <- function(formula){
  stopifnot(hasBracket(formula))
  out <- regmatches(deparse(formula), regexpr("\\[.*?\\]", deparse(formula)))
  names(out) <- as.character(formula[[2]])
  out
}

extractAllBrackets <- function(formula){
  if(hasBracket(formula, recursive=FALSE)){
    out <- extractBracket(formula)
  } else{
    if(is.call(formula)){
      out <- lapply(formula, extractAllBrackets)
    } else {
      out <- NULL
    }
  }
  out <- unlist(out)
  if(is.call(out) | is.numeric(out)) out <- list(out) # always return a list
  return(out)
}

getDataForLP <- function(components, form, LHS_ind){

  # Get data components by splitting on interaction term if it's present
  dat <- strsplit(components, ":")

  # Extract range indices from original formula
  inds <- extractAllBrackets(form)
  if(is.null(inds)){
    inds <- replicate(LHS_ind[[1]], n=length(all.vars(form)))
    names(inds) <- all.vars(form)
  }
  inds <- removeDuplicateIndices(inds)

  # Put indices back in data now that it's split
  dat <- putIndicesBackInData(dat, inds)

  # Generate final data list by combining with *
  dat <- sapply(dat, function(x) paste0(x, collapse=" * "))
  # remove intercept from data if it's present
  dat[grepl("(Intercept)", dat)] <- NA
  dat
}

# If parameter appears more than once in formula, make sure the indices
# match, then remove the duplicates
removeDuplicateIndices <- function(inds){
  for (i in names(inds)){
    subinds <- inds[names(inds)==i]
    stopifnot(all(subinds == subinds[[1]]))
    inds <- inds[unique(names(inds))]
  }
  inds
}

# Put the index brackets back in now that everything is split out
putIndicesBackInData <- function(dat, inds){
  lapply(dat, function(x, inds){
    if(x[1] == "(Intercept)") return(x)
    sapply(x, function(y, inds){
      ind_match <- sapply(names(inds), function(z) grepl(paste0("^",z), y))
      if(sum(ind_match) != 1){
        stop("Missing bracket index on RHS of formula", call.=FALSE)
      }
      paste0(y, inds[ind_match])
    }, inds=inds)
  }, inds=inds)
}

#' @export
linPred <- list(
  process = function(code, .constants, .env){
    RHS <- getRHS(code)
    prefix <- RHS$prefix
    if(is.null(prefix)){
      prefix <- quote(beta_)
    } else {
      RHS$prefix <- NULL 
    }   
    if(RHS[[1]] != quote(`~`)) RHS[[1]] <- quote(`~`)
    form <- as.formula(RHS)
    LHS_ind <- extractBracket(getLHS(code))

    newConstants <- splitFactorsInConstants(.constants)
    mm_cols <- modelMatrixCols(form, newConstants)

    pars <- getParametersForLP(mm_cols, prefix)
    dat <- getDataForLP(mm_cols, form, LHS_ind)

    # Combine parameters and data
    out <- mapply(pars, dat, FUN=function(x, y){
      if(is.na(y)) return(x)
      paste(x, y, sep=" * ")
    })
    out <- str2lang(paste(out, collapse = " + "))
    out <- as.call(list(quote(forLoop), out))
    RHS(code) <- out
    list(code=code, constants=newConstants)
  }
)
class(linPred) <- "model_macro"
