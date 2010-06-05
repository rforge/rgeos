
TopologyFunc = function(spgeom, id, byid, func) {
    
    byid = as.logical(byid)
    if (is.na(byid)) 
        stop("Invalid value for byid, must be logical")
    
    curids = extractIDs(spgeom)
    if (is.null(id)) {
        if (byid)   id = curids
        else        id = "1"
    }
    id = as.character(id)
    
    if ( length(id) != length(unique(id)) )
        stop("Non-unique values for id ")
    
    if ( !(!byid && length(id) == 1) && !(byid && length(id) == length(curids)) )
        stop("Invalid number of values in id" ) 
    
    return( .Call(func, .RGEOS_HANDLE, spgeom, id, 0, byid, PACKAGE="rgeos") )
}

RGEOSEnvelope = function(spgeom, id = NULL, byid=FALSE) {

    return( TopologyFunc(spgeom,id,byid,"rgeos_envelope") ) 
}

RGEOSConvexHull = function(spgeom, id = NULL, byid=FALSE) {
    
    return( TopologyFunc(spgeom,id,byid,"rgeos_convexhull") ) 
}

RGEOSBoundary = function(spgeom, id = NULL, byid=FALSE) {
     
     return( TopologyFunc(spgeom,id,byid,"rgeos_boundary") ) 
}

RGEOSGetCentroid = function(spgeom, id = NULL, byid=FALSE) {

    return( TopologyFunc(spgeom,id,byid,"rgeos_getcentroid") ) 
}

RGEOSPointOnSurface = function(spgeom, id = NULL, byid=FALSE) {

    return( TopologyFunc(spgeom,id,byid,"rgeos_pointonsurface") ) 
}

RGEOSLineMerge = function(spgeom, id = NULL, byid=FALSE) {

    return( TopologyFunc(spgeom,id,byid,"rgeos_linemerge") ) 
}


RGEOSSimplify = function(g1, tolerance) {}

RGEOSTopologyPreserveSimplify = function(g1, tolerance) {}

RGEOSPolygonize = function( geoms, ngeoms) {}

RGEOSPolygonizer_getCutEdges = function(geoms, ngeoms) {}





