RGEOSBinPredFunc = function(spgeom1, spgeom2, byid, func, optparam=NULL) {
    byid = as.logical(byid)
    if (any(is.na(byid)) ) stop("Invalid value for byid, must be logical")

    if( length(byid) < 1 || length(byid) > 2 )
        stop("Invalid length for byid, must be of length 1 or 2")

    if (length(byid) == 1)
        byid <- rep(byid,2)

    if(!is.null(spgeom1) & !is.null(spgeom2)) {
        if(!identical(spgeom1@proj4string,spgeom2@proj4string))
            warning("spgeom1 and spgeom2 have different proj4 strings")
    }

    if ( func == "rgeos_equalsexact"  ) {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, optparam, byid, PACKAGE="rgeos")
    } else {
        x <- .Call(func, .RGEOS_HANDLE, spgeom1, spgeom2, byid, PACKAGE="rgeos")
    }
    
    if(any(byid)) {
        id1 = row.names(spgeom1) 
        if (is.null(spgeom2)) id2 = id1
        else id2 = row.names(spgeom2)

        colnames(x) <- id1
        rownames(x) <- id2
    }
    
    return(x)
}


gDisjoint = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_disjoint") )
}
gTouches = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_touches") )
}
gIntersects = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_intersects") )
}
gCrosses = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_crosses") )
}
gWithin = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_within") )
}
gContains = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_contains") )
}
gOverlaps = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_overlaps") )
}
gEquals = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equals") )
}
gEqualsExact = function(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE) {
    
    if ( is.null(tol) ) 
        tol <- 0.0
    
    tol <- as.numeric(tol)
    if ( is.na(tol) ) 
        stop("Invalid value for tolerance, must be numeric")

    return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_equalsexact", tol) )
}
gRelate = function(spgeom1, spgeom2 = NULL, pattern = NULL, byid = FALSE) {
    
	if (is.null(pattern)) {
		return( RGEOSBinPredFunc(spgeom1,spgeom2,byid,"rgeos_relate") )
	} else {
		
	    if (length(pattern) != 1)
	        stop("Pattern must have length of 1")
    
	    if ( !is.character(pattern) || nchar(pattern) != 9 || !grepl("[0-2TF\\*]{9}",pattern) )
	        stop("Invalid pattern, see documentation for proper format")
    
	    return( RGEOSRelate(spgeom1, spgeom2, byid ) == pattern )
	}
}





RGEOSDisjoint = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gDisjoint")
    return( gDisjoint(spgeom1,spgeom2, byid) )
}
RGEOSTouches = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gTouches")
    return( gTouches(spgeom1,spgeom2, byid) )
}
RGEOSIntersects = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gIntersects")
    return( gIntersects(spgeom1,spgeom2, byid) )
}
RGEOSCrosses = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gCrosses")
    return( gCrosses(spgeom1,spgeom2, byid) )
}
RGEOSWithin = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gWithin")
    return( gWithin(spgeom1,spgeom2, byid) )
}
RGEOSContains = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gContains")
    return( gContains(spgeom1,spgeom2, byid) )
}
RGEOSOverlaps = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gOverlaps")
    return( gOverlaps(spgeom1,spgeom2, byid) )
}
RGEOSEquals = function(spgeom1, spgeom2 = NULL, byid = FALSE) {
    .Deprecated("gEquals")
    return( gEquals(spgeom1,spgeom2, byid) )
}
RGEOSEqualsExact = function(spgeom1, spgeom2 = NULL, tol=0.0, byid = FALSE) {
    .Deprecated("gEqualsexact")
    return( gEqualsExact(spgeom1,spgeom2,tol, byid) )
}
RGEOSRelate = function(spgeom1, spgeom2 = NULL, pattern = NULL, byid = FALSE) {
    .Deprecated("gRelate")
    return( gRelate(spgeom1,spgeom2,pattern, byid) )
}
