RGEOSUnaryPredFunc = function(spgeom, byid, func) {
    byid = as.logical(byid)
    if (is.na(byid)) stop("Invalid value for byid, must be logical")

    x <- .Call(func, .RGEOS_HANDLE, spgeom, byid, PACKAGE="rgeos")
    
    if(byid) {
        id <- unique(row.names(spgeom))
        names(x) <- id
    }
    return(x)
}

gEmpty = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isempty") )
}
gSimple  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_issimple") )
}
gRing  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isring") )
}
gHasZ  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_hasz") )
}
gValid  = function(spgeom, byid = FALSE, reason=FALSE) {
	if (reason) 
		return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalidreason") )	    	
	else
		return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalid") )
}

RGEOSisEmpty = function(spgeom, byid = FALSE) { 
    .Deprecated("gEmpty")
    return( gEmpty(spgeom, byid) )
}
RGEOSisSimple  = function(spgeom, byid = FALSE) { 
    .Deprecated("gSimple")
    return( gSimple(spgeom, byid) )
}
RGEOSisRing  = function(spgeom, byid = FALSE) { 
    .Deprecated("gRing")
    return( gRing(spgeom, byid) )
}
RGEOSHasZ  = function(spgeom, byid = FALSE) { 
    .Deprecated("gHasZ")
    return( gHasZ(spgeom, byid) )
}
RGEOSisValid  = function(spgeom, byid = FALSE, reason=FALSE) {
    .Deprecated("gValid")
    return( gValid(spgeom, byid, reason) )
}