
setClass("Ring", 
    representation(coords = "matrix",ID = "character"),
    validity = function(object) {
        if (any(is.na(object@coords)))
            stop("coords cannot contain missing values")
        if (ncol(object@coords) != 2)
            stop("coords should have 2 columns")
        if (!all(object@coords[1,] == object@coords[nrow(object@coords),]))
            stop("invalid ring, start and end coordinates must be equal")
        return(TRUE)
    }
)


setClass("SpatialRings",
    representation("Spatial", rings = "list"),
    
    prototype = list(bbox = matrix( rep(NA, 2), 2, 2, dimnames = list(NULL, c("min","max"))),
                                    proj4string = CRS(as.character(NA)),
                                    rings = list()),
    
    validity = function(object) {
        if (any(unlist(lapply(object@rings, function(x) !is(x, "Ring"))))) 
            stop("rings not Ring objects")
        if (any(duplicated(sapply(slot(object, "rings"), function(i) slot(i, "ID")))))
            return("non-unique Rings ID slot values")
        return(TRUE)
    }
)

Ring <- function(coords,ID=as.character(NA)) {
    if (ncol(coords) != 2) 
        stop("coords must be a two-column matrix")
    
    n = nrow(coords)
    area2 = sum( (coords[-1,1]-coords[-n,1])*(coords[-1,2]+coords[-n,2]) )
    if (area2 < 0) {
        # if area2 is negative coordinates are ccw, reverse them
        coords[,1] = rev(coords[,1])
        coords[,2] = rev(coords[,2])
    }
    
    coords <- coordinates(coords)
    new("Ring", coords = coords, ID = ID)
}

.bboxSRs <- function(lst) {
    bb = sapply(lst, bbox)
    res = matrix(c(min(bb[1,]), min(bb[2,]), max(bb[3,]), max(bb[4,])), 2, 2)
    dimnames(res) = list(c("x", "y"), c("min", "max"))
    return(res)
}

SpatialRings <- function(RingList, proj4string=CRS(as.character(NA))) {
    if (any(sapply(RingList, function(x) !is(x, "Ring")))) 
        stop("Ring list not exclusively filled with Ring objects")
    Sp <- new("Spatial", bbox = .bboxSRs(RingList), proj4string=proj4string)
    res <- new("SpatialRings", Sp, rings=RingList)
    return(res)
}


bbox.Ring <- function(obj) {
    rx <- range(obj@coords[,1])
    ry <- range(obj@coords[,2])
    res = rbind(r1 = rx, r2 = ry)
    dimnames(res)[[2]] <- c("min", "max")
    return(res)
}
setMethod("bbox", "Ring", bbox.Ring)


plotSpatialRings <- function(SR, xlim = NULL, ylim = NULL,
                             col = 1, lwd = 1, lty=1, add = FALSE, axes = FALSE, ..., 
                             setParUsrBB=FALSE) {

    if (!add) 
        plot(as(SR, "Spatial"), xlim = xlim, ylim = ylim, axes = axes, ..., setParUsrBB=setParUsrBB)
    

    lst <- SR@rings
    if (length(col) != length(lst)) 
        col <- rep(col[1], length(lst))
    if (length(lwd) != length(lst)) 
        lwd <- rep(lwd[1], length(lst))
    if (length(lty) != length(lst)) 
        lty <- rep(lty[1], length(lst))

    for (i in seq(along=lst)) {
        crds <- coordinates(lst[[i]])
        lines(crds, col = col[i], lwd = lwd[i], lty = lty[i], ...)
    }
}

setMethod("plot", 
          signature(x = "SpatialRings", y = "missing"), 
          function(x, y, ...) plotSpatialRings(x, ...) )

setMethod("coordinates", "Ring", function(obj) obj@coords)
setMethod("coordinates", "SpatialRings", function(obj) lapply(obj@rings, coordinates))


#if (!isGeneric("lines"))
#    setGeneric("lines", function(x, y, ...)
#        standardGeneric("lines"))

#setMethod("lines", "Ring", function(x, y = NULL, ...) invisible(lines(coordinates(x), ...)))
#setMethod("lines", "SpatialRings", function(x, y = NULL, ...) {
#    f = function(x, ...) lines(x, ...)
#    invisible(lapply(x@rings, f, ...))
#})

setMethod("row.names", "SpatialRings", function(x) sapply(slot(x, "rings"), slot, "ID"))
setReplaceMethod("row.names", signature(x = "SpatialRings", value = "character"),
                 function(x, value) spChFIDs(x, value))

setMethod("[", "SpatialRings", 
    function(x, i, j, ..., drop = TRUE) {
        if (any(is.na(i))) stop("NAs not permitted in row index")
        
        if (is.logical(i)) {
            if (length(i) == 1 && i) {
                i = 1:length(x@rings)
            } else {
                i <- which(i)
            }
        } else if (is.character(i)) {
            i <- match(i, row.names(x))
        }
        
        x@rings = x@rings[i]
        x@bbox = .bboxSRs(x@rings)
        return(x)
    }
)


setMethod("coordnames", signature(x = "SpatialRings"), function(x) coordnames(x@rings[[1]]))
setMethod("coordnames", signature(x = "Ring"), function(x) dimnames(coordinates(x))[[2]])

setReplaceMethod("coordnames", 
                 signature(x = "SpatialRings", value = "character"),
                 function(x, value) {
                     dimnames(x@bbox)[[1]] = value
                     for (i in seq(along = x@rings))
                        coordnames(x@rings[[i]]) = value
                     return(x)
                 }
)
setReplaceMethod("coordnames",signature(x = "Ring", value = "character"),
                 function(x, value) {
                     dimnames(x@coords)[[2]] = value
                     return(x)
                 }
)



chFIDsSpatialRings <- function(obj, x) {
    nl <- length(slot(obj, "rings"))
    if (length(x) != nl) stop("lengths differ")
    if (length(x) > length(unique(x))) stop("duplicate IDs")

    rings <- slot(obj, "rings")
    for (i in 1:nl) obj@rings[[i]]@ID = x[i]
    
    return(obj)
}
setMethod("spChFIDs", signature(obj="SpatialRings", x="character"), chFIDsSpatialRings)

setAs("Ring", 
      "SpatialPoints",
      function(from) { 
          SpatialPoints(do.call("rbind", coordinates(from)))
      }
)

setAs("SpatialRings", 
      "SpatialPoints", 
      function(from) { 
          SpatialPoints(do.call("rbind", 
                        lapply(from@rings, function(x) as(x, "SpatialPoints"))),
                        CRS(proj4string(from)))
      }
)
