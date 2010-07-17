gSimplify = function(g1, tol, topologyPreserve=FALSE) {}

gPolygonize = function( geoms, ngeoms) {}

gPolygonizer_getCutEdges = function(geoms, ngeoms) {}




TopologyFunc = function(spgeom, id, byid, func) {
    
    byid = as.logical(byid)
    if (is.na(byid)) 
        stop("Invalid value for byid, must be logical")
    
    curids = unique(row.names(spgeom))
    if (is.null(id)) {
        if (byid)   id = curids
        else        id = "1"
    }
    id = as.character(id)
    
    if ( length(id) != length(unique(id)) )
        stop("Non-unique values for id ")
    
    if ( !(!byid && length(id) == 1) && !(byid && length(id) == length(curids)) )
        stop("Invalid number of values in id" ) 
    
    return( .Call(func, .RGEOS_HANDLE, spgeom, id, byid, PACKAGE="rgeos") )
}

gEnvelope = function(spgeom, id = NULL, byid=FALSE) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_envelope") ) 
}
gConvexHull = function(spgeom, id = NULL, byid=FALSE) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_convexhull") ) 
}
gBoundary = function(spgeom, id = NULL, byid=FALSE) {
     return( TopologyFunc(spgeom,id,byid,"rgeos_boundary") ) 
}
gCentroid = function(spgeom, id = NULL, byid=FALSE) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_getcentroid") ) 
}
gPointOnSurface = function(spgeom, id = NULL, byid=FALSE) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_pointonsurface") ) 
}
gLineMerge = function(spgeom, id = NULL, byid=FALSE) {
    return( TopologyFunc(spgeom,id,byid,"rgeos_linemerge") ) 
}
gUnionCascaded = function(spgeom, id = NULL) {
    
    if (!inherits(spgeom,"SpatialPolygons"))
        stop("Invalid geometry, may only be applied to polygons")

    if (is.null(id))
        id = rep("1",length(row.names(spgeom)))

    return( TopologyFunc(groupID(spgeom,id),unique(id),TRUE,"rgeos_unioncascaded") ) 
}



RGEOSEnvelope = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gEnvelope")
    return( gEnvelope(spgeom, id, byid) )
}
RGEOSConvexHull = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gConvexHull")
    return( gConvexHull(spgeom, id, byid) )
}
RGEOSBoundary = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gBoundary")
    return( gBoundary(spgeom, id, byid) )
}
RGEOSGetCentroid = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gCentroid")
    return( gCentroid(spgeom, id, byid) )
}
RGEOSPointOnSurface = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gPointOnSurface")
    return( gPointOnSurface(spgeom, id, byid) )
}
RGEOSLineMerge = function(spgeom, id = NULL, byid=FALSE) {
    .Deprecated("gLineMerge")
    return( gLineMerge(spgeom, id, byid) )
}
RGEOSUnionCascaded = function(spgeom, id = NULL) {
    .Deprecated("gUnionCascaded")
    return( gUnionCascaded(spgeom, id) )
}




