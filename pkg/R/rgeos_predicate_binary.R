RGEOSBinPredFunc = function(spgeom1, spgeom2, byid, func, tol=NULL) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid, must be logical")

    if (is.null(spgeom2))
        spgeom2 = spgeom1
    
    if (is.null(tol)) {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    } else {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, tol, byid, PACKAGE="rgeos")
    }
    
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


RGEOSDisjoint = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_disjoint") )
}

RGEOSTouches = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_touches") )
}

RGEOSIntersects = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_intersects") )
}

RGEOSCrosses = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_crosses") )
}

RGEOSWithin = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_within") )
}

RGEOSContains = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_contains") )
}

RGEOSOverlaps = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_overlaps") )
}

RGEOSEquals = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equals") )
}

RGEOSEqualsExact = function(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equalsexact", tol) )
}