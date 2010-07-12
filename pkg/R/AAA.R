.RGEOS_HANDLE <- new.env(FALSE, parent=globalenv())

.onLoad <- function(lib, pkg) {
  require(methods, quietly = TRUE, warn.conflicts = FALSE)
  require("sp")
  require("stringr")
  library.dynam('rgeos', pkg, lib)

  GEOSptr <- .Call('rgeos_Init', PACKAGE="rgeos")
  assign("GEOSptr", GEOSptr, envir=.RGEOS_HANDLE)
  assign("scale", 100000000, envir=.RGEOS_HANDLE)
  svn_version <- scan(system.file("SVN_VERSION", package="rgeos"),
    what=character(1), sep="\n", quiet=TRUE)
  cat("rgeos: (SVN ", svn_version, ")\n", sep="")
  cat("GEOS runtime version:", .Call("rgeos_GEOSversion", PACKAGE="rgeos"),"\n")
}

.onUnload <- function(libpath) {
  invisible(.Call('rgeos_finish', .RGEOS_HANDLE, PACKAGE="rgeos"))
}
