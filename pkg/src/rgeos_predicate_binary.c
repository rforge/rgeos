#include "rgeos.h"

SEXP rgeos_disjoint(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSDisjoint_r) );
}

SEXP rgeos_touches(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSTouches_r) );
}

SEXP rgeos_intersects(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSIntersects_r) );
}

SEXP rgeos_crosses(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSCrosses_r) );
}

SEXP rgeos_within(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSWithin_r) );
}

SEXP rgeos_contains(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSContains_r) );
}

SEXP rgeos_overlaps(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSOverlaps_r) );
}

SEXP rgeos_equals(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSEquals_r) );
}

SEXP rgeos_relate(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid) {
    return( rgeos_binpredfunc(env,spgeom1,spgeom2,byid, &GEOSRelate_r) );
}

SEXP rgeos_binpredfunc(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP byid, 
                       char (*binpredfunc)(GEOSContextHandle_t, const GEOSGeom, const GEOSGeom)) {

    SEXP ans, dims;
    GEOSGeom geom1, curgeom1;
    GEOSGeom geom2, curgeom2;
    
    int val;
    int sym_ans = FALSE;
    int i,j, m, n, pc=0;
    int type1, type2;
    char *buf;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    m = 1;
    n = 1;
    if (spgeom2 == R_NilValue) {
        sym_ans = TRUE;
        geom1 = rgeos_convert_R2geos(env, spgeom1);
        type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

        geom2 = geom1;

        if (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) {
            m = GEOSGetNumGeometries_r(GEOShandle, geom1);
            n = m;
        }
    } else {
        geom1 = rgeos_convert_R2geos(env, spgeom1);
        type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

        geom2 = rgeos_convert_R2geos(env, spgeom2);
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);

        if (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION)
            m = GEOSGetNumGeometries_r(GEOShandle, geom1);

        if (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION)
            n = GEOSGetNumGeometries_r(GEOShandle, geom2);
    }
    
    
    if (m == -1) error("rgeos_binpredfunc: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_binpredfunc: invalid number of subgeometries in geometry 2");
    
    if (binpredfunc == GEOSRelate_r) {
        PROTECT(ans = NEW_CHARACTER(m*n)); pc++;
    } else {
        PROTECT(ans = NEW_LOGICAL(m*n)); pc++;
    }
    
    curgeom1 = geom1;
    curgeom2 = geom2;
    for(i=0; i<m; i++) {
        
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_binpredfunc: unable to get subgeometries from geometry 1");
        }
        for(j=0; j<n; j++) {
            if(sym_ans && j > i)
                break;
            
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_binpredfunc: unable to get subgeometries from geometry 2");
            }
            
            if (binpredfunc == GEOSRelate_r) {
                buf = (char *) GEOSRelate_r(GEOShandle, curgeom1, curgeom2);
                if (buf == NULL)
                    error("rgeos_isvalidreason: test failed");

                SET_STRING_ELT(ans, n*i+j, COPY_TO_USER_STRING(buf));
                if (sym_ans)
                    SET_STRING_ELT(ans, n*j+i, COPY_TO_USER_STRING(buf));

                GEOSFree_r(GEOShandle, buf);
            } else {
                val = (int) binpredfunc(GEOShandle, curgeom1, curgeom2);
                if (val == 2)
                    error("rgeos_binpredfunc: comparison failed");

                LOGICAL_POINTER(ans)[n*i+j] = val;
                if (sym_ans)
                    LOGICAL_POINTER(ans)[n*j+i] = val;
            }
        }
    }
    
    if (LOGICAL_POINTER(byid)[0] || LOGICAL_POINTER(byid)[1]) {
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!sym_ans)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}


// TODO - generalize with rgeos_relatepattern
SEXP rgeos_equalsexact(SEXP env, SEXP spgeom1, SEXP spgeom2, SEXP tol, SEXP byid) {
    SEXP ans, dims;
    GEOSGeom geom1, curgeom1;
    GEOSGeom geom2, curgeom2;
    
    int val;
    int sym_ans = FALSE;
    int i,j, m, n, pc=0;
    int type1, type2;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    m = 1;
    n = 1;
    if (spgeom2 == R_NilValue) {
        sym_ans = TRUE;
        geom1 = rgeos_convert_R2geos(env, spgeom1);
        type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

        geom2 = geom1;

        if (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION) {
                m = GEOSGetNumGeometries_r(GEOShandle, geom1);
                n = m;
        }
    } else {
        geom1 = rgeos_convert_R2geos(env, spgeom1);
        type1 = GEOSGeomTypeId_r(GEOShandle, geom1);

        geom2 = rgeos_convert_R2geos(env, spgeom2);
        type2 = GEOSGeomTypeId_r(GEOShandle, geom2);

        if (LOGICAL_POINTER(byid)[0] && type1 == GEOS_GEOMETRYCOLLECTION)
                m = GEOSGetNumGeometries_r(GEOShandle, geom1);

        if (LOGICAL_POINTER(byid)[1] && type2 == GEOS_GEOMETRYCOLLECTION)
                n = GEOSGetNumGeometries_r(GEOShandle, geom2);
    }
    
    if (m == -1) error("rgeos_equalsexact: invalid number of subgeometries in geometry 1");
    if (n == -1) error("rgeos_equalsexact: invalid number of subgeometries in geometry 2");
    
    PROTECT(ans = NEW_LOGICAL(m*n)); pc++;
    
    curgeom1 = geom1;
    curgeom2 = geom2;
    for(i=0; i<m; i++) {
        
        if ( m > 1) {
            curgeom1 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom1, i);
            if (curgeom1 == NULL) 
                error("rgeos_equalsexact: unable to get subgeometries from geometry 1");
        }
        for(j=0; j<n; j++) {
            if(sym_ans && j > i)
                break;
            
            if ( n > 1) {
                curgeom2 = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom2, j);
                if (curgeom2 == NULL) 
                    error("rgeos_equalsexact: unable to get subgeometries from geometry 2");
            }
            
            val = (int) GEOSEqualsExact_r(GEOShandle, curgeom1, curgeom2, NUMERIC_POINTER(tol)[0] );
            if (val == 2)
                error("rgeos_equalsexact: comparison failed");

            LOGICAL_POINTER(ans)[n*i+j] = val;
            if (sym_ans)
                LOGICAL_POINTER(ans)[n*j+i] = val;
            
        }
    }
    
    if (n != 1 && m !=1) {
        PROTECT(dims = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dims)[0] = n;
        INTEGER_POINTER(dims)[1] = m;
        setAttrib(ans, R_DimSymbol, dims);
    }
    
    GEOSGeom_destroy_r(GEOShandle, geom1);
    if (!sym_ans)
        GEOSGeom_destroy_r(GEOShandle, geom2);
    
    UNPROTECT(pc);
    return(ans);
}


