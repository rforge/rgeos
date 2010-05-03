.RGEOS_HANDLE <- new.env(FALSE, parent=globalenv())

.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  library.dynam('rgeos', pkg, lib)

  GEOSptr <- .Call('rgeos_Init', PACKAGE="rgeos")
  assign("GEOSptr", GEOSptr, envir=.RGEOS_HANDLE)
  assign("scale",100000000,envir=.RGEOS_HANDLE)
  
  cat("GEOS runtime version:", .Call("rgeos_GEOSversion", PACKAGE="rgeos"),"\n")
}

.onUnload <- function(libpath) {
  invisible(.Call('rgeos_finish', .RGEOS_HANDLE, PACKAGE="rgeos"))
}
