#include "rgeos.h"

SEXP rgeos_buffer(SEXP env, SEXP obj, SEXP id, SEXP thres, SEXP width, SEXP quadsegs) {
    
    SEXP ans, p4s;
    GEOSGeom geom, res;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));
    
    res = GEOSBuffer_r(GEOShandle, geom, NUMERIC_POINTER(width)[0], INTEGER_POINTER(quadsegs)[0]);
    
    ans = rgeos_convert_geos2R(env, res, p4s, id, thres);
    return(ans);
}