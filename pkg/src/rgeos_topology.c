#include "rgeos.h"


SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSEnvelope_r) );
}

SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSConvexHull_r) );
}

SEXP rgeos_boundary(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSBoundary_r) );
}
    
SEXP rgeos_getcentroid(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSGetCentroid_r) );
}

SEXP rgeos_pointonsurface(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSPointOnSurface_r) );
}

SEXP rgeos_linemerge(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSLineMerge_r) );
}

SEXP rgeos_unioncascaded(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid ) {
    return( rgeos_topologyfunc(env, obj, id, thres, byid, &GEOSUnionCascaded_r) );  
}

SEXP rgeos_topologyfunc(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid, 
                        GEOSGeom (*topofunc)(GEOSContextHandle_t, const GEOSGeom) ) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    SEXP p4s = GET_SLOT(obj, install("proj4string"));
    GEOSGeom geom = rgeos_convert_R2geos(env, obj);
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    int n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    if (n < 1) error("rgeos_topologyfunc: invalid number of geometries");
    
    GEOSGeom *resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    GEOSGeom curgeom = geom;
    int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
    for(int i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_topologyfunc: unable to get subgeometries");
            curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        }
        
        if (topofunc == GEOSUnionCascaded_r && curtype == GEOS_POLYGON) {
            resgeoms[i] = curgeom;
        } else {
            resgeoms[i] = topofunc(GEOShandle, curgeom);
            if (resgeoms[i] == NULL)
                error("rgeos_topologyfunc: unable to calculate");
        }


    }
    
    GEOSGeom res = resgeoms[0];
    if (n > 1)
        res = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, n);
    
    //FIXME - crashes if geom created from a collection created by R_Alloc
    //GEOSGeom_destroy_r(GEOShandle, geom);
        
    return( rgeos_convert_geos2R(env, res, p4s, id, thres) );
}
