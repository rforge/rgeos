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

RGEOSisEmpty = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isempty") )
}

RGEOSisValid  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalid") )
}

RGEOSisSimple  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_issimple") )
}

RGEOSisRing  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isring") )
}

RGEOSHasZ  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_hasz") )
}

RGEOSisValidReason  = function(spgeom, byid = FALSE) { 
    return( RGEOSUnaryPredFunc(spgeom, byid,"rgeos_isvalidreason") )
}