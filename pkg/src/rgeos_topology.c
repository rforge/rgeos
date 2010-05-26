#include "rgeos.h"

SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    
    SEXP ans;
    GEOSGeom geom, envelope;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));
     
    envelope = GEOSEnvelope_r(GEOShandle, geom);
    
    ans = rgeos_convert_geos2R(env, envelope, p4s, id, thres);
    return(ans);
}

SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP thres) {

    SEXP ans;
    GEOSGeom geom, envelope;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));

    envelope = GEOSConvexHull_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, envelope, p4s, id, thres);
    return(ans);
}