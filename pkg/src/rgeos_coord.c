#include "rgeos.h"

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim) {

    unsigned int i, n, m;
    n = (unsigned int) INTEGER_POINTER(dim)[0];
    m = (unsigned int) INTEGER_POINTER(dim)[1];
    double val, scale = getScale(env);

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (m != 2) error("Only 2D geometries permitted");

    GEOSCoordSeq s;

    s = GEOSCoordSeq_create_r(GEOShandle, n, m);

    for(i=0; i<n; i++) {
        val = makePrecise( NUMERIC_POINTER(mat)[i], scale);
        if (GEOSCoordSeq_setX_r(GEOShandle, s, i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: X not set for %d", i);
        }
        val = makePrecise( NUMERIC_POINTER(mat)[i+n], scale);
        if (GEOSCoordSeq_setY_r(GEOShandle, s, i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: Y not set for %d", i);
        }
    }

    return(s);

}

GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_crdMat2CoordSeq(env, mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLineString_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_crdMat2LineString: lineString not created");
    }
    return(gl);
}


GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_crdMat2CoordSeq(env, mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLinearRing_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_crdMat2LinearRing: linearRing not created");
    }
    if ((int) GEOSisValid_r(GEOShandle, gl) == 1) {
        if (GEOSNormalize_r(GEOShandle, gl) == -1)
            warning("rgeos_crdMat2LinearRing: normalization failure");
    } else {
        warning("rgeos_crdMat2LinearRing: validity failure");
    }

    return(gl);
}

GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim) {

    GEOSGeom g1, p1;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    g1 = rgeos_crdMat2LinearRing(env, mat, dim);

    if ((p1 = GEOSGeom_createPolygon_r(GEOShandle, g1, NULL,
        (unsigned int) 0)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, g1);
        error("rgeos_crdMat2Polygon: Polygon not created");
    }

    return(p1);

}


SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev) {

    int pc=0, i, n, m, ii;
    double val,scale = getScale(env);

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        return(R_NilValue);
    if (GEOSCoordSeq_getDimensions_r(GEOShandle, s, &m) == 0)
        return(R_NilValue);
    if (m == 3 && HasZ == 1)
        warning("rgeos_CoordSeq2crdMat: only 2D coordinates respected");
    
    SEXP ans, dims;
    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = n;
    INTEGER_POINTER(dims)[1] = 2;

    for (i=0; i<n; i++){
        ii = (rev) ? (n-1)-i : i;
        if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }    

        NUMERIC_POINTER(ans)[ii] = makePrecise( val, scale);

        if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }

        NUMERIC_POINTER(ans)[ii+n] = makePrecise(val, scale);
    }

    setAttrib(ans, R_DimSymbol, dims);
    UNPROTECT(pc);
    return(ans);

}


SEXP rgeos_MP2crdMat(SEXP env, GEOSGeom GC) {

    int pc=0;
    unsigned int i, n;
    SEXP ans, dims, dimnames;
    GEOSGeom pt;
    GEOSCoordSeq s;
    double val, scale=getScale(env);
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    n = (unsigned int) GEOSGetNumGeometries_r(GEOShandle, GC);


    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = (int) n;
    INTEGER_POINTER(dims)[1] = (int) 2;
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("y"));

    if (n == 1) {

            if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle,GC)) == NULL) {
                return(R_NilValue);
            }
            if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0, &val) == 0) {
                return(R_NilValue);
            }    

            NUMERIC_POINTER(ans)[0] = makePrecise(val, scale);

            if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0, &val) == 0) {
                return(R_NilValue);
            }

            NUMERIC_POINTER(ans)[1] = makePrecise(val, scale);
        

    } else {
        for (i=0; i<n; i++) {

            if ((pt = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC, (int) i)) == NULL) {
                return(R_NilValue);
            }
            if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, pt)) == NULL) {
                return(R_NilValue);
            }
            if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0, &val) == 0) {
                return(R_NilValue);
            }    

            NUMERIC_POINTER(ans)[i] = makePrecise(val, scale);

            if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0, &val) == 0) {
                return(R_NilValue);
            }

            NUMERIC_POINTER(ans)[i+n] = makePrecise(val, scale);
        }
    }

    setAttrib(ans, R_DimSymbol, dims);
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);
}

GEOSCoordSeq rgeos_xy2CoordSeq(SEXP env, double x, double y) {


    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = GEOSCoordSeq_create_r(GEOShandle, (unsigned int) 1, (unsigned int) 2);

    if (GEOSCoordSeq_setX_r(GEOShandle, s, 0, x) == 0) {
        GEOSCoordSeq_destroy_r(GEOShandle, s);
        error("rgeos_xy2CoordSeq: X not set");
    }
    if (GEOSCoordSeq_setY_r(GEOShandle, s, 0, y) == 0) {
        GEOSCoordSeq_destroy_r(GEOShandle, s);
        error("rgeos_xy2CoordSeq: Y not set");
    }

    return(s);

}

GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_xy2CoordSeq(env, x, y);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createPoint_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_xy2Pt: point not created");
    }
    return(gl);

}

