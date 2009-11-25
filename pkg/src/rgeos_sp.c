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

GEOSGeom rgeos_Polygons2GC(SEXP obj) {

    SEXP pls, crdMat, dim, comm;
    GEOSGeom *geoms;
    GEOSGeom Pol, GC;
    int npls, i, pc=0;

    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    npls = length(pls);

    PROTECT(comm = comment2comm(obj)); pc++;

    if (comm == R_NilValue) {

        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (i=0; i<npls; i++) {
            crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(crdMat, R_DimSymbol);
            Pol = rgeos_crdMat2Polygon(crdMat, dim);
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, geoms, 
            npls)) == NULL) {
            error("Polygons2GC: collection not created");
        }
    } else {

        int nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        for (i=0; i<nErings; i++) {
            Pol = rgeos_Polygons_i_2Polygon(pls, VECTOR_ELT(comm, i));
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, geoms, 
            nErings)) == NULL) {
            error("Polygons2GC: collection not created");
        }
    }

    UNPROTECT(pc);
    return(GC);

}

GEOSGeom rgeos_Polygons_i_2Polygon(SEXP pls, SEXP vec) {

    GEOSGeom res, pol, hole;
    GEOSGeom *holes;
    SEXP mat, dim;

    int n = length(vec);
    int i, j;

    i = INTEGER_POINTER(vec)[0]-R_OFFSET;

    mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
    dim = getAttrib(mat, R_DimSymbol);
    pol = rgeos_crdMat2LinearRing(mat, dim);
    if (n == 1) {
        if ((res = GEOSGeom_createPolygon(pol, NULL, (unsigned int) 0))
            == NULL) {
            GEOSGeom_destroy(pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    } else {
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(mat, R_DimSymbol);
            hole = rgeos_crdMat2LinearRing(mat, dim);
            holes[(j-1)] = hole;
        }
        if ((res = GEOSGeom_createPolygon(pol, holes, (unsigned int) (n-1)))
            == NULL) {
            GEOSGeom_destroy(pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    }
    return(res);
}

SEXP rgeos_PolygonsContain(SEXP obj) {

    SEXP ans, dim;
    int pc=0;
    unsigned int i, j, n;
    int contains;

    GEOSGeom GC, Pi, Pj;

    GC = rgeos_Polygons2GC(obj);

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

SEXP rgeos_SpatialPolygonsSimplify(SEXP obj, SEXP tolerance) {

    double tol=NUMERIC_POINTER(tolerance)[0];
    GEOSGeom in, out;
    int pc=0, npls, i;
    SEXP ans, p4s, pls, IDs;

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);
    PROTECT(IDs = NEW_CHARACTER(npls)); pc++;
/*    for (i=0; i<npls; i++) {
        SET_STRING_ELT(IDs, i, GET_SLOT(VECTOR_ELT(pls, i), install("ID")));
    }*/

    in = rgeos_SpatialPolygonsGC(obj);

Rprintf("n_geoms: %d\n", GEOSGetNumGeometries(in));
    if ((out = (GEOSGeometry *) GEOSTopologyPreserveSimplify(in, tol))
        == NULL) {
            GEOSGeom_destroy(in);
            return(R_NilValue);
    }

    PROTECT(ans = rgeos_GCSpatialPolygons(out, p4s, IDs)); pc++;
    GEOSGeom_destroy(in);
    GEOSGeom_destroy(out);
Rprintf("completed\n");
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_GCSpatialPolygons(GEOSGeom Geom, SEXP p4s, SEXP IDs) {
    SEXP ans, pls;
    int pc=0, ng, i;
    GEOSGeom GC;

    ng = GEOSGetNumGeometries(Geom);

    PROTECT(pls = NEW_LIST(ng)); pc++;
    for (i=0; i<ng; i++) {
        GC = (GEOSGeometry *) GEOSGetGeometryN(Geom, i);
/*Rprintf("type[%d]: %s\n", i, GEOSGeomType(GC));*/
        SET_VECTOR_ELT(pls, i, rgeos_GCPolygons(GC, STRING_ELT(IDs, i)));
    }

    PROTECT(ans = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(ans, 0, pls);
/*    INTEGER_POINTER(VECTOR_ELT(ans, 0))[0] = ng;*/
    SET_VECTOR_ELT(ans, 1, rgeos_Geom2bbox(Geom));

    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_GCPolygons(GEOSGeom Geom, SEXP ID) {
    SEXP ans, pls;
    int pc=0, ng, i, nps=0;
    GEOSGeom GC;

    if (GEOSGeomTypeId(Geom) == GEOS_POLYGON) {
        nps = GEOSGetNumInteriorRings(Geom) + 1;
        PROTECT(pls = NEW_LIST(nps)); pc++;

    } else if (GEOSGeomTypeId(Geom) == GEOS_MULTIPOLYGON) {

        ng = GEOSGetNumGeometries(Geom);

        for (i=0; i<ng; i++) {
            GC = (GEOSGeometry *) GEOSGetGeometryN(Geom, i);
            nps = nps + (GEOSGetNumInteriorRings(GC) + 1);
        }
        PROTECT(pls = NEW_LIST(nps)); pc++;

    }
    PROTECT(ans = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(ans)[0] = nps;

    GEOSGeom_destroy(GC);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_GCPolygon(GEOSGeom Geom) {

}

GEOSGeom rgeos_SpatialPolygonsGC(SEXP obj) {

    SEXP pls;
    int npls,i , pc=0;
    GEOSGeom *geoms;
    GEOSGeom GC;

    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);

    geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

    for (i=0; i<npls; i++)
        geoms[i] = rgeos_Polygons2GC(VECTOR_ELT(pls, i));

    if ((GC = GEOSGeom_createCollection(GEOS_GEOMETRYCOLLECTION, geoms,
        npls)) == NULL) {
            error("rgeos_SpatialPolygonsGC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}

SEXP comment2comm(SEXP obj) {
    SEXP ans, comment, comm;
    int pc=0, ns, i, j, jj, k;
    char buf[BUFSIZE], s[15];
    int *c, *nss, *co, *coo;

    PROTECT(comment = getAttrib(obj, install("comment"))); pc++;
    if (comment == R_NilValue) {
        UNPROTECT(pc);
        return(R_NilValue);
    }
    strcpy(buf, CHAR(STRING_ELT(comment, 0)));
    ns = i = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') ns++;
        ++i;
    }
    k = (int) strlen(buf);
   
    nss = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    c = (int *) R_alloc((size_t) (ns+1), sizeof(int));
    i = j = 0;
    while (buf[i] != '\0') {
        if (buf[i] == ' ') {
            nss[j] = i; ++j;
        }
        ++i;
    }
    nss[(ns)] = k;
       
    strncpy(s, &buf[0], (size_t) nss[0]);
    c[0] = atoi(s);
    for (i=0; i<ns; i++) {
        k = nss[(i+1)]-(nss[i]+1);
        strncpy(s, &buf[(nss[i]+1)], (size_t) k);
        s[k] = '\0';
        c[i+1] = atoi(s);
    }

    for (i=0, k=0; i<(ns+1); i++) if (c[i] == 0) k++;
    
    PROTECT(ans = NEW_LIST((k))); pc++;
    co = (int *) R_alloc((size_t) k, sizeof(int));
    coo = (int *) R_alloc((size_t) k, sizeof(int));
    for (i=0; i<k; i++) co[i] = 1;

    for (i=0, j=0; i<(ns+1); i++)
        if (c[i] == 0) coo[j++] = i + R_OFFSET;

    for (i=0; i<k; i++)
        for (j=0; j<(ns+1); j++)
            if ((c[j]) == coo[i]) co[i]++;

    for (i=0; i<k; i++) SET_VECTOR_ELT(ans, i, NEW_INTEGER(co[i]));

    for (i=0; i<k; i++) {
        jj = 0;
        INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = coo[i];
        if (co[i] > 1) {
            for (j=0; j<(ns+1); j++)
                if (c[j] == coo[i])
                    INTEGER_POINTER(VECTOR_ELT(ans, i))[jj++] = j + R_OFFSET;
        }
    }

    UNPROTECT(pc);
    return(ans);
}

