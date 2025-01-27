.onLoad <- function(libname, pkgname) {
  #message("Enabling model macros in nimbleOptions")
  nimble::nimbleOptions(enableMacros = TRUE)
  nimble::nimbleOptions(enableMacroComments = TRUE)
  invisible()
}
