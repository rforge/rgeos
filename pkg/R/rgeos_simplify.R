thinnedSpatialPolyGEOS <- function(SP, tolerance, minarea=0) {
    stopifnot(inherits(SP, "SpatialPolygons"))
    if (missing(tolerance)) tolerance <- 0.0
    tolerance <- as.double(tolerance)
    res <- .Call("rgeos_SpatialPolygonsSimplify", .RGEOS_HANDLE, SP, tolerance,
        as.double(minarea), PACKAGE="rgeos")
    if (is(SP, "SpatialPolygonsDataFrame"))
        res <- SpatialPolygonsDataFrame(res, data=slot(SP, "data"))
    res
}


