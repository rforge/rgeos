#include "rgeos.h"


SEXP rgeos_area(SEXP env, SEXP obj, SEXP byid) {

    return( rgeos_miscfunc(env, obj, byid, &GEOSArea_r) );
}


SEXP rgeos_length(SEXP env, SEXP obj, SEXP byid) {

    return( rgeos_miscfunc(env, obj, byid, &GEOSLength_r) );
}


SEXP rgeos_miscfunc(SEXP env, SEXP obj, SEXP byid, int (*miscfunc)(GEOSContextHandle_t, const GEOSGeom, double *) ) {

    SEXP ans;
    GEOSGeom geom, curgeom;
    double val;
    int i, n, type, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom = rgeos_convert_R2geos(env, obj);
    type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    n = 1;
    if (LOGICAL_POINTER(byid)[0] && type == GEOS_GEOMETRYCOLLECTION)
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    curgeom = geom;
    for(i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_miscfunc: unable to get subgeometries");
        }
        
        if (!miscfunc(GEOShandle, curgeom, &val))
            error("rgeos_miscfunc: unable to calculate");
            
        NUMERIC_POINTER(ans)[i] = val;
    }

    GEOSGeom_destroy_r(GEOShandle, geom);

    UNPROTECT(pc);
    return(ans);
}



SEXP rgeos_distance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {

    return( rgeos_distancefunc(env, spgeom1, spgeom2, byid, &GEOSDistance_r) );
}

SEXP rgeos_hausdorffdistance(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {

    return( rgeos_distancefunc(env, spgeom1, spgeom2, byid, &GEOSHausdorffDistance_r) );
}

SEXP rgeos_distancefunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, 
                        int (*distfunc)(GEOSContextHandle_t,const GEOSGeom,const GEOSGeom, double *)) {

    SEXP ans, dims;
    GEOSGeom geom1, curgeom1;
    GEOSGeom geom2, curgeom2;
    
    double dist;
    int i,j, m, n, pc=0;
    int type1, type2;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    geom1 = rgeos_convert_R2geos(env, spgeom1);
    type1 = GEOSGeomTypeId_r(GEOShandle, geom1);
    
    geom2 = rgeos_convert_R2geos(env, spgeom2);
    type2 = GEOSGeomTypeId_r(GEOShandle, geom2);
    
    m = 1;
    n = 1;
    if (LOGICAL_POINTER(byid)[0]) {
        if (type1 == GEOS_GEOMETRYCOLLECTION)
            m = GEOSGetNumGeometries_r(GEOShandle, geom1);
        
        if (type2 == GEOS_GEOMETRYCOLLECTION)
            n = GEOSGetNumGeometries_r(GEOShandle, geom2);
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
                    error("rgeos_distancefunc: unable to get subgeometries from geometry 2");
            }
            
            
            if (!distfunc(GEOShandle, curgeom1, curgeom2, &dist))
                error("rgeos_distancefunc: unable to calculate area");

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