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


SEXP rgeos_distance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {

    SEXP ans;
    ans = rgeos_calcdistance(env, spgeom1, spgeom2, byid, 0);
    return(ans);
}

SEXP rgeos_hausdorffdistance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {

    SEXP ans;
    ans = rgeos_calcdistance(env, spgeom1, spgeom2, byid, 1);
    return(ans);
}

SEXP rgeos_calcdistance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, int hausdorff) {

    SEXP ans, dims;
    GEOSGeom geom1, curgeom1;
    GEOSGeom geom2, curgeom2;
    
    double dist;
    int i,j, m, n, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int (*distfunc)(GEOSContextHandle_t,const GEOSGeom,const GEOSGeom, double *);
    if (hausdorff) {
        distfunc = GEOSHausdorffDistance_r;
    } else {
        distfunc = GEOSDistance_r;
    }

    geom1 = rgeos_convert_R2geos(env, spgeom1);
    geom2 = rgeos_convert_R2geos(env, spgeom2);

    if (LOGICAL_POINTER(byid)[0]) {
        m = GEOSGetNumGeometries_r(GEOShandle, geom1);
        n = GEOSGetNumGeometries_r(GEOShandle, geom2);
    } else {
        m = 1;
        n = 1;
    }
    
    PROTECT(ans = NEW_NUMERIC(m*n)); pc++;

    curgeom1 = geom1;
    curgeom2 = geom2;
    for(i=0; i<m; i++) {
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_distance: unable to get subgeometries from geometry 1");
        }
        for(j=0; j<n; j++) {
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_distance: unable to get subgeometries from geometry 2");
            }
            
            
            if (!distfunc(GEOShandle, curgeom1, curgeom2, &dist))
                error("rgeos_distance: unable to calculate area");

            NUMERIC_POINTER(ans)[n*i+j] = dist;
        }
    }
    
    if (n != 1 && m !=1) {
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}