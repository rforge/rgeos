#include "rgeos.h"

SEXP rgeos_relatepattern(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_RELATEPATTERN_FUNC) );
}

SEXP rgeos_disjoint(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_DISJOINT_FUNC) );
}

SEXP rgeos_touches(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_TOUCHES_FUNC) );
}

SEXP rgeos_intersects(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_INTERSECTS_FUNC) );
}

SEXP rgeos_crosses(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_CROSSES_FUNC) );
}

SEXP rgeos_within(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_WITHIN_FUNC) );
}

SEXP rgeos_contains(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_CONTAINS_FUNC) );
}

SEXP rgeos_overlaps(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_OVERLAPS_FUNC) );
}

SEXP rgeos_equals(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, GEOS_EQUALS_FUNC) );
}


SEXP rgeos_binpredfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, int funcid) {

    SEXP ans, dims;
    GEOSGeom geom1, curgeom1;
    GEOSGeom geom2, curgeom2;
    
    int val;
    int i,j, m, n, pc=0;
    int type1, type2;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    char (*binpredfunc)(GEOSContextHandle_t,const GEOSGeom,const GEOSGeom);
    
    switch(funcid) {
        //case GEOS_RELATEPATTERN_FUNC:
        //    binpredfunc = GEOSRelatePattern_r;
        //    break;
        case GEOS_DISJOINT_FUNC:
            binpredfunc = GEOSDisjoint_r;
            break;
        case GEOS_TOUCHES_FUNC:
            binpredfunc = GEOSTouches_r;
            break;
        case GEOS_INTERSECTS_FUNC:
            binpredfunc = GEOSIntersects_r;
            break;
        case GEOS_CROSSES_FUNC:
            binpredfunc = GEOSCrosses_r;
            break;
        case GEOS_WITHIN_FUNC:
            binpredfunc = GEOSWithin_r;
            break;
        case GEOS_CONTAINS_FUNC:
            binpredfunc = GEOSContains_r;
            break;
        case GEOS_OVERLAPS_FUNC:
            binpredfunc = GEOSOverlaps_r;
            break;
        case GEOS_EQUALS_FUNC:
            binpredfunc = GEOSEquals_r;
            break;
        default:
            error("rgeos_binpredfunc: invalid distance function");
    }


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
    
    PROTECT(ans = NEW_LOGICAL(m*n)); pc++;

    curgeom1 = geom1;
    curgeom2 = geom2;
    for(i=0; i<m; i++) {
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_binpredfunc: unable to get subgeometries from geometry 1");
        }
        for(j=0; j<n; j++) {
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_binpredfunc: unable to get subgeometries from geometry 2");
            }
            val = (int) binpredfunc(GEOShandle, curgeom1, curgeom2);
            if (val == 2)
                error("rgeos_binpredfunc: comparison failed");

            LOGICAL_POINTER(ans)[n*i+j] = val;
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