## gpclib:  General Polygon Clipping library for R
## Copyright (C) 2003-2004 Roger D. Peng <rpeng@jhsph.edu>


## R functions for using GPC library and manipulating polygons

## Compute the area of each polygon in the polygon set contained in
## `object'.  

areaGPC <- function(x.mat) {
    if(nrow(x.mat) < 3) 
        return(0);   
    x.segmat <- cbind(x.mat, rbind(x.mat[2:nrow(x.mat), ],
         x.mat[1, ]));
    abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3]
        * x.segmat[,2])) / 2
}

setClass("gpc.poly", representation(pts = "list"))
setMethod("show", "gpc.poly",
          function(object) {
              cat("GPC Polygon\n")
              cat("   Num. Contours: ", length(object@pts), "\n")
              if(length(object@pts) == 1)
                  cat("   Num. Vertices: ", length(object@pts[[1]]$x),"\n")
              bbox <- get.bbox(object)
              cat("   BBox (X): ", bbox$x[1], "-->", bbox$x[2], "\n")
              cat("   BBox (Y): ", bbox$y[1], "-->", bbox$y[2], "\n")
              invisible(object)
          })

setGeneric("get.bbox", function(x)
           standardGeneric("get.bbox"))

setMethod("get.bbox", signature(x = "gpc.poly"),
          function(x) {
              pts <- x@pts
              x <- unlist(lapply(pts, "[[", "x"))
              y <- unlist(lapply(pts, "[[", "y"))
              
              if(!is.null(x))
                  xlim <- range(x)
              else
                  xlim <- c(NA, NA)
              if(!is.null(y))
                  ylim <- range(y)
              else
                  ylim <- c(NA, NA)
              list(x = xlim, y = ylim)
          })

setGeneric("plot")


plotGpcPoly <- function(x, y, poly.args = list(), xlab = "X", ylab = "Y",
                   asp = 1, add = FALSE, ...) {
              if(!add) {
                  bbox <- get.bbox(x)
                  plot(0, 0, ylim = bbox$y, xlim = bbox$x, type="n",
                       xlab = xlab, ylab = ylab, asp = asp, ...)
              }
              areas <- sapply(1:length(slot(x, "pts")),
                 function(i) area.poly(x[i]))
              o <- order(areas, decreasing=TRUE)
              gpcs <- slot(x, "pts")[o]
              holes <- sapply(gpcs, "[[", "hole")
              col <- poly.args[["col"]]
              if (is.null(col)) {
                  col <- "transparent"
              } else {
                  npa <- names(poly.args)
                  ncol <- grep("col", npa)
                  poly.args <- poly.args[-ncol]
              }
              for (i in seq(along=gpcs)) {
                  if (holes[i]) coli <- "white"
                  else coli <- col
                  args = append(list(x=gpcs[[i]], col=coli), poly.args)
                  do.call("polygon", args)
              }
                            
#              invisible(lapply(x@pts, function(p) {
#                  do.call("polygon", append(list(x = p), poly.args))
#              }))
          }

setMethod("plot", "gpc.poly", plotGpcPoly)

setGeneric("intersect")
setGeneric("union")
setGeneric("setdiff")

if (!isGeneric("symdiff")) setGeneric("symdiff", function(x, y)
		standardGeneric("symdiff"))

setMethod("intersect", signature(x = "gpc.poly", y = "gpc.poly"),
          function(x, y) {
              gpclist1 <- slot(x, "pts")
              gpclist2 <- slot(y, "pts")
              vec <- IntersectGpcGEOS(gpclist1, gpclist2)
              if(length(vec) == 0)
                  rval <- new("gpc.poly")
              else 
                  rval <- new("gpc.poly", pts=vec)
              rval
          })

setMethod("setdiff", signature(x = "gpc.poly", y = "gpc.poly"),
          function(x, y) {
              gpclist1 <- slot(x, "pts")
              gpclist2 <- slot(y, "pts")
              vec <- DiffGpcGEOS(gpclist1, gpclist2)
              if(length(vec) == 0)
                  rval <- new("gpc.poly")
              else 
                  rval <- new("gpc.poly", pts=vec)
              rval
          })

setMethod("symdiff", signature(x = "gpc.poly", y = "gpc.poly"),
          function(x, y) {
              gpclist1 <- slot(x, "pts")
              gpclist2 <- slot(y, "pts")
              vec <- SymDiffGpcGEOS(gpclist1, gpclist2)
              if(length(vec) == 0)
                  rval <- new("gpc.poly")
              else 
                  rval <- new("gpc.poly", pts=vec)
              rval
          })

setMethod("union", signature(x = "gpc.poly", y = "gpc.poly"),
          function(x, y) {
              gpclist1 <- slot(x, "pts")
              gpclist2 <- slot(y, "pts")
              vec <- UnionGpcGEOS(gpclist1, gpclist2)
              if(length(vec) == 0)
                  rval <- new("gpc.poly")
              else                   
                  rval <- new("gpc.poly", pts=vec)
              rval
          })

setMethod("[", "gpc.poly",
          function(x, i, j, ..., drop = FALSE) {
              new("gpc.poly", pts = x@pts[i])
          })

setAs("matrix", "gpc.poly",
      function(from, to) {
          if(ncol(from) > 2)
              stop("Matrix must have 2 columns")
          p <- list(x = from[,1], y = from[,2], hole = FALSE)
          new("gpc.poly", pts = list(p))
      })

setAs("data.frame", "gpc.poly",
      function(from, to) {
          as(as.matrix(from), "gpc.poly")
      })

setGeneric("append.poly", function(x, y)
           standardGeneric("append.poly"))

setMethod("append.poly",
          signature(x = "gpc.poly", y = "gpc.poly"),
          function(x, y) {
              newpts <- append(x@pts, y@pts)
              new("gpc.poly", pts = newpts)
          })

setGeneric("scale.poly", function(x, ...)
           standardGeneric("scale.poly"))

setMethod("scale.poly", signature(x = "gpc.poly"), 
          function(x, xscale, yscale = xscale, ...) {
              x@pts <- lapply(x@pts, function(p) {
                  p$x <- p$x / xscale
                  p$y <- p$y / yscale
                  p
              })
              x
          })

setGeneric("area.poly", function(object, ...)
           standardGeneric("area.poly"))

setMethod("area.poly", signature(object = "gpc.poly"),
          function(object, ...) {
              area <- function(x.mat) {
                  if(nrow(x.mat) < 3) 
                      return(0);   
                  x.segmat <- cbind(x.mat, rbind(x.mat[2:nrow(x.mat), ],
                                                 x.mat[1, ]));
                  abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3]
                          * x.segmat[,2])) / 2
              }
              if(length(object@pts) == 0)
                  return(0)
              a <- sapply(object@pts, function(p) area(cbind(p$x, p$y)))
              holeflags <- sapply(object@pts, "[[", "hole")
              sum(a[!holeflags]) - sum(a[holeflags])
          })


