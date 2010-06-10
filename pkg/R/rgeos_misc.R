RGEOSMiscFunc = function(spgeom, byid, func) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid, must be logical")
    
    x <- .Call("rgeos_area", .RGEOS_HANDLE, spgeom, byid, PACKAGE="rgeos")
    if(byid) names(x) <- extractIDs(spgeom)
    
    return(x)
}

RGEOSArea = function(spgeom, byid=FALSE) {

    return( RGEOSMiscFunc(spgeom,byid,"rgeos_area") )
}

RGEOSLength = function(spgeom, byid=FALSE) {

    return(RGEOSMiscFunc(spgeom,byid,"rgeos_length"))
}


RGEOSDistanceFunc = function(spgeom1, spgeom2, byid, func) {
    byid = as.logical(byid)
    if (any(is.na(byid)) ) stop("Invalid value for byid, must be logical")

    if( length(byid) < 1 || length(byid) > 2 )
        stop("Invalid length for byid, must be of length 1 or 2")

    if (length(byid) == 1)
        byid <- rep(byid,2)

    x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    
    if(all(byid)) {
        id1 = row.names(spgeom1) 
        if (is.null(spgeom2)) id2 = id1
        else id2 = row.names(spgeom2)
        
        if ( length(id1) != 1 && length(id2) == 1 ) {
            names(x) <- id1       
        } else if ( length(id1) == 1 && length(id2) != 1 ) {
            names(x) <- id2
        } else if ( length(id1) != 1 && length(id2) != 1 ) {
            colnames(x) <- id1
            rownames(x) <- id2
        }
    } else if (byid[1]) {
        names(x) = row.names(spgeom1)
    } else if (byid[2]) {
        if (is.null(spgeom2)) names(x) = row.names(spgeom1)
        else names(x) = row.names(spgeom2)
    }
    return(x)
} 

RGEOSDistance = function(spgeom1, spgeom2=NULL, byid=FALSE) {
    
    return( RGEOSDistanceFunc(spgeom1, spgeom2, byid, "rgeos_distance") )
}


RGEOSisWithinDistance = function(spgeom1, spgeom2=NULL, dist, byid=FALSE) {
    #TODO - include a tolerance?
    return( RGEOSDistance(spgeom1,spgeom2,byid) <= dist )
}

RGEOSHausdorffDistance = function(spgeom1, spgeom2=NULL, byid=FALSE) {

    return( RGEOSDistanceFunc(spgeom1, spgeom2, byid, "rgeos_hausdorffdistance") )
}

RGEOSHausdorffDistanceDensify = function(g1, g2, densifyFrac) {}
