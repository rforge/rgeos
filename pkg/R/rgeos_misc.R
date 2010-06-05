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
    if (is.na(byid)) 
        stop("Invalid value for byid, must be logical")
    
    if (is.null(spgeom2))
        spgeom2 = spgeom1
    
    x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    
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
