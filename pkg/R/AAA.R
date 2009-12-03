.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  library.dynam('rgeos', pkg, lib)

  .Call('rgeos_Init', PACKAGE="rgeos")
  cat("GEOS runtime version:", .Call("rgeos_GEOSversion", PACKAGE="rgeos"), "\n")
}

.onUnload <- function(libpath) {
  invisible(.Call('rgeos_finish', PACKAGE="rgeos"))
}
