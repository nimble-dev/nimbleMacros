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
  spatialModel[[1]] <- substitute(nimbleMacros::MOD, list(MOD = spatialModel[[1]]))

  # Manually set prefix for spatial parameter terms
  spatialModel$prefix <- str2lang(paste0(coefPrefix, mod_name, "_"))

  spatial_re <- str2lang(paste0(coefPrefix, mod_name))
  spatial_re <- substitute(SPAT[IDX], list(SPAT=spatial_re, IDX=spatialIndex))
  
  spat_code <- substitute(SPATRE ~ SPATMOD,
                          list(SPATRE = spatial_re, SPATMOD = spatialModel))

  embedLinesInCurlyBrackets(list(code, spat_code))
}
