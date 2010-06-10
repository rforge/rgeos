RGEOSBinPredFunc = function(spgeom1, spgeom2, byid, func, optparam=NULL) {
    byid = as.logical(byid)
    if (any(is.na(byid)) ) stop("Invalid value for byid, must be logical")

	if( length(byid) < 1 || length(byid) > 2 )
		stop("Invalid length for byid, must be of length 1 or 2")
	
	if (length(byid) == 1)
		byid <- rep(byid,2)


    if ( func == "rgeos_equalsexact"  ) {
        tol <- optparam
        
        if ( is.null(tol) ) tol <- 0.0
        tol <- as.numeric(tol)
        if ( is.na(tol) ) stop("Invalid value for tolerance, must be numeric")
        
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, tol, byid, PACKAGE="rgeos")
        
    } else {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    }
    
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