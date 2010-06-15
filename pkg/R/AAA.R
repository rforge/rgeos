.RGEOS_HANDLE <- new.env(FALSE, parent=globalenv())

.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  require("stringr")
  library.dynam('rgeos', pkg, lib)

  GEOSptr <- .Call('rgeos_Init', PACKAGE="rgeos")
  assign("GEOSptr", GEOSptr, envir=.RGEOS_HANDLE)
  assign("scale", 100000000, envir=.RGEOS_HANDLE)
  cat("rgeos: version", packageDescription("rgeos")$Version,
    "(svn revision $Rev$)\n")
  cat("GEOS runtime version:", .Call("rgeos_GEOSversion", PACKAGE="rgeos"),"\n")
}

.onUnload <- function(libpath) {
  invisible(.Call('rgeos_finish', .RGEOS_HANDLE, PACKAGE="rgeos"))
}
