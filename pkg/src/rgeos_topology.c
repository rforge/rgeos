#include "rgeos.h"

SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    
    SEXP ans, p4s;
    GEOSGeom geom, res;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));
     
    res = GEOSEnvelope_r(GEOShandle, geom);
    
    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}

SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP thres) {

    SEXP ans, p4s;
    GEOSGeom geom, res;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));

    res = GEOSConvexHull_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}


SEXP rgeos_boundary(SEXP env, SEXP obj, SEXP id, SEXP thres) {

    SEXP ans, p4s;
    GEOSGeom geom, res;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));

    res = GEOSBoundary_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}
    
SEXP rgeos_getcentroid(SEXP env, SEXP obj, SEXP id, SEXP thres) {

    SEXP ans, p4s;
    GEOSGeom geom, res;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));

    res = GEOSGetCentroid_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}

SEXP rgeos_pointonsurface(SEXP env, SEXP obj, SEXP id, SEXP thres) {

    SEXP ans, p4s;
    GEOSGeom geom, res;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));

    res = GEOSPointOnSurface_r(GEOShandle, geom);

    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}