
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


RGEOSDistance = function(spgeom1, spgeom2, byid=FALSE) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid")

    x <- .Call("rgeos_distance", .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    
    if(byid) {
        id1 <- extractIDs(spgeom1)
        id2 <- extractIDs(spgeom2)

        if ( length(id1) != 1 && length(id2) == 1 )
            names(x) <- id1        
        if ( length(id1) == 1 && length(id2) != 1 )
            names(x) <- id2
        if ( length(id1) != 1 && length(id2) != 1 ) {
            colnames(x) <- id1
            rownames(x) <- id2
        }
        
    }
    return(x)
}


RGEOSisWithinDistance = function(spgeom1, spgeom2, dist, byid=FALSE) {
    return( RGEOSDistance(spgeom1,spgeom2,byid) <= dist )
}

RGEOSHausdorffDistance = function(g1, g2) {}

RGEOSHausdorffDistanceDensify = function(g1, g2, densifyFrac) {}
