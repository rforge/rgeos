RGEOSEnvelope = function(spgeom, id = "envelope") {
    x = .Call("rgeos_envelope", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x) 
}

RGEOSConvexHull = function(spgeom, id="convexhull") {
    x = .Call("rgeos_convexhull", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x)
}

RGEOSBoundary = function(spgeom, id="boundary") {
    x = .Call("rgeos_boundary", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x)
}

RGEOSGetCentroid = function(spgeom, id="centroid") {
    x = .Call("rgeos_getcentroid", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x)
}

RGEOSPointOnSurface = function(spgeom, id="1") {
    x = .Call("rgeos_pointonsurface", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x)
}

RGEOSSimplify = function(g1, tolerance) {}

RGEOSTopologyPreserveSimplify = function(g1, tolerance) {}

RGEOSPolygonize = function( geoms, ngeoms) {}

RGEOSPolygonizer_getCutEdges = function(geoms, ngeoms) {}


RGEOSLineMerge = function(g) {}


