
RGEOSArea = function(spgeom, byid=FALSE) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid")
    
    x <- .Call("rgeos_area", .RGEOS_HANDLE, spgeom, byid, PACKAGE="rgeos")
    if(byid) names(x) <- extractIDs(spgeom)
    
    return(x)
}

RGEOSLength = function(spgeom, byid=FALSE) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid")

    x <- .Call("rgeos_length", .RGEOS_HANDLE, spgeom, byid, PACKAGE="rgeos")
    if(byid) names(x) <- extractIDs(spgeom)
    
    return(x)
}


RGEOSDistance = function(g1, g2) {}

RGEOSisWithinDistance = function(g1,g2,dist) {
    return( RGEOSDistance(g1,g2) <= dist )
}

RGEOSHausdorffDistance = function(g1, g2) {}

RGEOSHausdorffDistanceDensify = function(g1, g2, densifyFrac) {}
