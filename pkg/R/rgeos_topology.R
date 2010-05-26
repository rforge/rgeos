RGEOSEnvelope = function(spgeom, id = "envelope") {
    x = .Call("rgeos_envelope", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x) 
}

RGEOSConvexHull = function(spgeom, id="convexhull") {
    x = .Call("rgeos_convexhull", .RGEOS_HANDLE, spgeom, id, 0, PACKAGE="rgeos")
    return(x)
}

RGEOSBoundary = function(g1) {}

RGEOSGetCentroid = function(g) {}

RGEOSSimplify = function(g1, tolerance) {}

RGEOSTopologyPreserveSimplify = function(g1, tolerance) {}

RGEOSPolygonize = function( geoms, ngeoms) {}

RGEOSPolygonizer_getCutEdges = function(geoms, ngeoms) {}

RGEOSPointOnSurface = function(g1) {}

RGEOSLineMerge = function(g) {}


