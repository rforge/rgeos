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

    if ((p1 = GEOSGeom_createPolygon_r(GEOShandle, g1, NULL, (unsigned int) 0)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, g1);
        error("rgeos_crdMat2Polygon: Polygon not created");
    }

    return(p1);

}


SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev) {

    int pc=0, i, n, m, ii;
    double val,scale = getScale(env);
    SEXP ans;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        return(R_NilValue);
    if (GEOSCoordSeq_getDimensions_r(GEOShandle, s, &m) == 0)
        return(R_NilValue);
    if (m == 3 && HasZ == 1)
        warning("rgeos_CoordSeq2crdMat: only 2D coordinates respected");
    
    
    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;

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

    PROTECT(ans = rgeos_formatcrdMat(ans,n));pc++;
    
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geospoint2crdMat(SEXP env, GEOSGeom geom, int ntotal, int type) {

    int i,j,k=0;
    int m,n,pc=0;
    int subtype;
    SEXP ans;
    GEOSGeom subgeom, subsubgeom;
    GEOSCoordSeq s;
    double x, y, scale=getScale(env);
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(ans = NEW_NUMERIC(ntotal*2)); pc++;

    m = GEOSGetNumGeometries_r(GEOShandle, geom);
    if (m == -1) return(R_NilValue);
    
    for(j = 0; j<m; j++) {
        
        if (type == GEOS_POINT) {
            subgeom = geom;
            n = 1;
            
        } else if (type == GEOS_MULTIPOINT || type == GEOS_GEOMETRYCOLLECTION) {
            if ((subgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j)) == NULL)
                return(R_NilValue);
                
            n = GEOSGetNumGeometries_r(GEOShandle, subgeom);
            
            subtype = GEOSGeomTypeId_r(GEOShandle, subgeom);
            
            if(subtype != GEOS_POINT && subtype != GEOS_MULTIPOINT)
                return(R_NilValue); //Subgeometry must be either point or multipoint
        } else {
            return(R_NilValue); //Geometry must be point, multipoint, or geometrycollection
        }
        
        if (n == -1) return(R_NilValue);

        for (i=0; i<n; i++) {
        
            if (type == GEOS_GEOMETRYCOLLECTION && subtype == GEOS_MULTIPOINT) {
                if ((subsubgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, subgeom, i)) == NULL)
                    return(R_NilValue);
            } else {
                subsubgeom = subgeom; // if subtype is a point we dont need to get subsubgeom
            }
           
            if ((s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, subsubgeom)) == NULL)
                return(R_NilValue);
        
            if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0, &x) == 0 ||
                GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0, &y) == 0 ) {
                
                return(R_NilValue);
            }
            
            NUMERIC_POINTER(ans)[k]        = makePrecise(x, scale);
            NUMERIC_POINTER(ans)[k+ntotal] = makePrecise(y, scale);
    
            GEOSCoordSeq_destroy_r(GEOShandle,s);
            k++;
        }
    }
    
    PROTECT(ans = rgeos_formatcrdMat(ans,ntotal));pc++;
    
    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_formatcrdMat( SEXP crdMat, int n ) {
    SEXP dims, dimnames;
    int pc = 0;
    
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = n;
    INTEGER_POINTER(dims)[1] = 2;
    
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("y"));
    
    setAttrib(crdMat, R_DimSymbol, dims);
    setAttrib(crdMat, R_DimNamesSymbol, dimnames);
    
    UNPROTECT(pc);
    return(crdMat);
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

