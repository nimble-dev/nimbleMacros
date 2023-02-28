#' @importFrom nimble nimbleOptions
.onLoad <- function(libname, pkgname) {
  #message("Enabling model macros in nimbleOptions")
  nimble::nimbleOptions(enableModelMacros = TRUE)
  invisible()
}
