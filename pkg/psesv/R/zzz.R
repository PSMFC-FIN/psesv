.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("psesv", system.file("www", package = pkgname))
}