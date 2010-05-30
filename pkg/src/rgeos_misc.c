#include "rgeos.h"


SEXP rgeos_area(SEXP env, SEXP obj, SEXP byid) {

    SEXP ans;
    GEOSGeom geom, curgeom;
    double area;
    int i, n, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);

    if (LOGICAL_POINTER(byid)[0])
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    else
        n = 1;
    
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    curgeom = geom;
    for(i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_area: unable to get subgeometries");
        }
        
        if (!GEOSArea_r(GEOShandle, curgeom, &area))
            error("rgeos_area: unable to calculate area");
            
        NUMERIC_POINTER(ans)[i] = area;
    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_length(SEXP env, SEXP obj, SEXP byid) {

    SEXP ans;
    GEOSGeom geom, curgeom;
    double len;
    int i, n, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);

    if (LOGICAL_POINTER(byid)[0])
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    else
        n = 1;
    
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    curgeom = geom;
    for(i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_length: unable to get subgeometries");
        }
        
        if (!GEOSLength_r(GEOShandle, curgeom, &len))
            error("rgeos_length: unable to calculate area");
            
        NUMERIC_POINTER(ans)[i] = len;
    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    UNPROTECT(pc);
    return(ans);
}