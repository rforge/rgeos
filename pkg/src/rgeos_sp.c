#include "rgeos.h"

GEOSGeom rgeos_SPoints2MP(SEXP obj) {

    int pc=0;
    unsigned int i, n;
    GEOSGeom *geoms;
    GEOSGeom pt, GC;
    SEXP crds, dim;

    crds = GET_SLOT(obj, install("coords")); 
    dim = getAttrib(crds, install("dim")); 
    n = INTEGER_POINTER(dim)[0];

    geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));

    for (i=0; i<n; i++) {
        pt = rgeos_xy2Pt(NUMERIC_POINTER(crds)[i], NUMERIC_POINTER(crds)[i+n]);
        geoms[i] = pt;
    }

    if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOINT, geoms, n)) == NULL) {
        error("rgeos_SPoints2MP: collection not created");
    }

    return(GC);
}

GEOSGeom rgeos_Polygons2GC(SEXP obj, SEXP comm) {

    SEXP pls, crdMat, dim;
    GEOSGeom *geoms;
    GEOSGeom Pol, GC;
    int npls, i, pc=0;

    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    npls = length(pls);
    if (comm == R_NilValue) {

        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (i=0; i<npls; i++) {
            crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(crdMat, R_DimSymbol);
            Pol = rgeos_crdMat2Polygon(crdMat, dim);
            geoms[i] = Pol;
        }
    }/* else {

        GEOSGeom g1, hole;
        GEOSGeom *holes;
        int nErings = length(comm);

        g1 = rgeos_crdMat2LinearRing(mat, dim);


    }*/

    if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, geoms, 
        npls)) == NULL) {
        error("Polygons2GC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);

}

SEXP rgeos_PolygonsContain(SEXP obj, SEXP comm) {

    SEXP ans, dim;
    int pc=0;
    unsigned int i, j, n;
    int contains;

    GEOSGeom GC, Pi, Pj;

    GC = rgeos_Polygons2GC(obj, comm);

    if (!((int) GEOSisValid(GC))) {
        n = (unsigned int) GEOSGetNumGeometries(GC);
        PROTECT(ans = NEW_LOGICAL((int) (n*n))); pc++;
        PROTECT(dim = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dim)[0] = (int) n;
        INTEGER_POINTER(dim)[1] = (int) n;
        setAttrib(ans, R_DimSymbol, dim);

        for (i=0; i<n; i++) {
            if ((Pi = (GEOSGeometry *) GEOSGetGeometryN(GC, (int) i))
                 == NULL) {
                GEOSGeom_destroy(GC);
                return(R_NilValue);
            }
                for (j=0; j<n; j++) {
                    if ((Pj = (GEOSGeometry *) GEOSGetGeometryN(GC, (int) j))
                        == NULL) {
                        GEOSGeom_destroy(GC);
                        return(R_NilValue);
                    }
                    if (i == j) LOGICAL_POINTER(ans)[i+(j*n)] = FALSE;
                    else {
                        contains = (int) GEOSContains(Pi, Pj);
                        if (contains == 2) 
                            LOGICAL_POINTER(ans)[i+(j*n)] = NA_LOGICAL;
                        else
                            LOGICAL_POINTER(ans)[i+(j*n)] = contains;
                    }
                }

        }
        GEOSGeom_destroy(GC);

        UNPROTECT(pc);
        return(ans);
    }
    
    GEOSGeom_destroy(GC);

    return(R_NilValue);
}

SEXP rgeos_MP2crdMat(GEOSGeom GC) {

    int pc=0;
    unsigned int i, n;
    SEXP ans, dims, dimnames;
    GEOSGeom pt;
    double val;
    GEOSCoordSeq s;

    n = (unsigned int) GEOSGetNumGeometries(GC);


    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = (int) n;
    INTEGER_POINTER(dims)[1] = (int) 2;
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("y"));

    for (i=0; i<n; i++) {

        if ((pt = (GEOSGeometry *) GEOSGetGeometryN(GC, (int) i)) == NULL) {
            return(R_NilValue);
        }
        if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq(pt)) == NULL) {
            return(R_NilValue);
        }
        if (GEOSCoordSeq_getX(s, (unsigned int) 0, &val) == 0) {
            return(R_NilValue);
        }    

        NUMERIC_POINTER(ans)[i] = val;

        if (GEOSCoordSeq_getY(s, (unsigned int) 0, &val) == 0) {
            return(R_NilValue);
        }

        NUMERIC_POINTER(ans)[i+n] = val;
    }

    setAttrib(ans, R_DimSymbol, dims);
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_inout(SEXP obj) {

    SEXP ans, p4s, bb, SPans;
    GEOSGeom GC, bbG;
    int pc=0;
    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;

    GC = rgeos_SPoints2MP(obj);

    PROTECT(ans = rgeos_MP2crdMat(GC)); pc++;
    if (ans == R_NilValue) error("MultiPoint to matrix conversion failed");

    PROTECT(bb = rgeos_Geom2bbox(GC)); pc++;
    if (bb == R_NilValue) error("Bounding box creation failed");

    PROTECT(SPans = NEW_OBJECT(MAKE_CLASS("SpatialPoints"))); pc++;
    SET_SLOT(SPans, install("coords"), ans);
    SET_SLOT(SPans, install("proj4string"), p4s);
    SET_SLOT(SPans, install("bbox"), bb);

    GEOSGeom_destroy(GC);

    UNPROTECT(pc);
    return(SPans);
}


SEXP rgeos_Geom2bbox(GEOSGeom Geom) {

    GEOSGeom bb, bbER;
    GEOSCoordSeq s;
    unsigned int i, n;
    double UX=DBL_MIN, LX=DBL_MAX, UY=DBL_MIN, LY=DBL_MAX;
    SEXP bbmat, ans, dim, dimnames;
    int pc=0;

    if ((bb = GEOSEnvelope(Geom)) == NULL) {
        return(R_NilValue);
    }

    if ((bbER = (GEOSGeometry *) GEOSGetExteriorRing(bb)) == NULL) {
        return(R_NilValue);
    }

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq(bbER)) == NULL) {
        return(R_NilValue);
    }
    GEOSCoordSeq_getSize(s, &n);
    if (n == 0) {
        return(R_NilValue);
    }
    
    bbmat = rgeos_CoordSeq2crdMat(s, (int) GEOSHasZ(bb)); 
    for (i=0; i<n; i++) {
       if (NUMERIC_POINTER(bbmat)[i] > UX) UX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] > UY) UY = NUMERIC_POINTER(bbmat)[i+n];
       if (NUMERIC_POINTER(bbmat)[i] < LX) LX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] < UY) LY = NUMERIC_POINTER(bbmat)[i+n];
    }

    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = LX;
    NUMERIC_POINTER(ans)[1] = LY;
    NUMERIC_POINTER(ans)[2] = UX;
    NUMERIC_POINTER(ans)[3] = UY;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(ans, R_DimSymbol, dim);
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_SpatialPolygonsSimplify(SEXP obj, SEXP tolerance, SEXP preserve) {

    double tol=NUMERIC_POINTER(tolerance)[0];
    GEOMGeom in, out;
    int pc=0;
    SEXP ans;

    in = rgeos_SpatialPolygonsGC(obj);
    if (LOGICAL_POINTER(preserve)[0]) {
        if ((out = (GEOSGeometry *) GEOSTopologyPreserveSimplify(in, tol))
            == NULL) {
            GEOSGeom_destroy(in);
            return(R_NilValue);
        }
    } else {
        if ((out = (GEOSGeometry *) GEOSSimplify(in, tol))
            == NULL) {
            GEOSGeom_destroy(in);
            return(R_NilValue);
        }
    }
    PROTECT(ans = rgeos_GCSpatialPolygons(out); pc++;
    GEOSGeom_destroy(in);
    GEOSGeom_destroy(out);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_GCSpatialPolygons(GEOSGeom Geom) {

}
