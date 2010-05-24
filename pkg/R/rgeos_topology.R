RGEOSEnvelope = function(obj, id = "envelope") {
    x = .Call("rgeos_envelope", .RGEOS_HANDLE, obj, id, 0, PACKAGE="rgeos")
    return(x) 
}

RGEOSConvexHull = function(g1) {}

RGEOSBoundary = function(g1) {}

RGEOSGetCentroid = function(g) {}

RGEOSSimplify = function(g1, tolerance) {}

RGEOSTopologyPreserveSimplify = function(g1, tolerance) {}

RGEOSPolygonize = function( geoms, ngeoms) {}

RGEOSPolygonizer_getCutEdges = function(geoms, ngeoms) {}

RGEOSPointOnSurface = function(g1) {}

RGEOSLineMerge = function(g) {}


