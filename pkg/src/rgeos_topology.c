#include "rgeos.h"

SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    
    int  pc = 0;
    SEXP ans, p4s;
    GEOSGeom geom, envelope;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    geom = rgeos_convert_R2geos(env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));
     
    envelope = GEOSEnvelope_r(GEOShandle, geom);
    
    ans = rgeos_convert_geos2R(env, envelope, p4s, id, thres);
    return(ans);
}

