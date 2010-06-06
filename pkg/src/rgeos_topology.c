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

SEXP rgeos_topologyfunc(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP byid, 
                        GEOSGeom (*topofunc)(GEOSContextHandle_t, const GEOSGeom) ) {

    SEXP ans, p4s;
    GEOSGeom geom, curgeom;
    GEOSGeom res, *resgeoms;
    double area;
    int i, n, type;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    p4s = GET_SLOT(obj, install("proj4string"));
    geom = rgeos_convert_R2geos(env, obj);
    type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    resgeoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    curgeom = geom;
    for(i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_topologyfunc: unable to get subgeometries");
        }
        
        resgeoms[i] = topofunc(GEOShandle, curgeom);
        if (resgeoms[i] == NULL)
            error("rgeos_topologyfunc: unable to calculate area");

    }
    
    if (n==1) {
        res = resgeoms[0];
    } else {
        res = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, resgeoms, n);
    }
    
    //FIXME - crashes if geom created from a collection created by R_Alloc
    //GEOSGeom_destroy_r(GEOShandle, geom);
        
    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);

    return(ans);
}
