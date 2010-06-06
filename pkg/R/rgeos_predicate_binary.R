RGEOSBinPredFunc = function(spgeom1, spgeom2, byid, func, optparam=NULL) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid, must be logical")

    
    if ( func == "rgeos_equalsexact"  ) {
        tol = optparam
        
        if ( is.null(tol) ) tol = 0.0
        tol = as.numeric(tol)
        if ( is.na(tol) ) stop("Invalid value for tolerance, must be numeric")
        
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, tol, byid, PACKAGE="rgeos")
        
    } else {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    }
    
    if(byid) {
        id1 <- extractIDs(spgeom1)
        if (is.null(spgeom2))
            id2 <- id1
        else
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

RGEOSRelate = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_relate") )
}

RGEOSRelatePattern = function(spgeom1, spgeom2 = NULL, pattern, byid = FALSE) {
    
    if (length(pattern) != 1)
        stop("Pattern must have length of 1")
    
    if ( !is.character(pattern) || nchar(pattern) != 9 || !grepl("[0-2TF\\*]{9}",pattern) )
        stop("Invalid pattern, see documentation for proper format")
    
    return( RGEOSRelate(spgeom1, spgeom2, byid ) == pattern )
}