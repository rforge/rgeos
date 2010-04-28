#include "rgeos.h"

GEOSGeom rgeos_SPoints2MP(SEXP env, SEXP obj) {

    int pc=0;
    unsigned int i, n;
    GEOSGeom *geoms;
    GEOSGeom pt, GC;
    SEXP crds, dim;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    crds = GET_SLOT(obj, install("coords")); 
    dim = getAttrib(crds, install("dim")); 
    n = INTEGER_POINTER(dim)[0];

    geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));

    for (i=0; i<n; i++) {
        pt = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[i],
            NUMERIC_POINTER(crds)[i+n]);
        geoms[i] = pt;
    }

    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT,
        geoms, n)) == NULL) {
        error("rgeos_SPoints2MP: collection not created");
    }

    return(GC);
}

GEOSGeom rgeos_Lines2GC(SEXP env, SEXP obj) {

    SEXP lns, crdMat, dim;
    GEOSGeom *geoms;
    GEOSGeom ls, GC;
    int nlns, i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(lns = GET_SLOT(obj, install("Lines"))); pc++;
    nlns = length(lns);


    geoms = (GEOSGeom *) R_alloc((size_t) nlns, sizeof(GEOSGeom));

    for (i=0; i<nlns; i++) {
        crdMat = GET_SLOT(VECTOR_ELT(lns, i), install("coords"));
        dim = getAttrib(crdMat, R_DimSymbol);
        ls = rgeos_crdMat2LineString(env, crdMat, dim);
        geoms[i] = ls;
    }
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTILINESTRING,
        geoms, nlns)) == NULL) {
        error("Lines2GC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);

}


GEOSGeom rgeos_Polygons2GC(SEXP env, SEXP obj) {

    SEXP pls, crdMat, dim, comm;
    GEOSGeom *geoms;
    GEOSGeom Pol, GC;
    int npls, i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    npls = length(pls);

    PROTECT(comm = SP_PREFIX(comment2comm)(obj)); pc++;

    if (comm == R_NilValue) {

        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (i=0; i<npls; i++) {
            crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(crdMat, R_DimSymbol);
            Pol = rgeos_crdMat2Polygon(env, crdMat, dim);
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON,
            geoms, npls)) == NULL) {
            error("Polygons2GC: collection not created");
        }
    } else {

        int nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        for (i=0; i<nErings; i++) {
            Pol = rgeos_Polygons_i_2Polygon(env, pls, VECTOR_ELT(comm, i));
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON,
            geoms, nErings)) == NULL) {
            error("Polygons2GC: collection not created");
        }
    }

    UNPROTECT(pc);
    return(GC);

}

GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec) {

    GEOSGeom res, pol, hole;
    GEOSGeom *holes;
    SEXP mat, dim;

    int n = length(vec);
    int i, j;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    i = INTEGER_POINTER(vec)[0]-R_OFFSET;

    mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
    dim = getAttrib(mat, R_DimSymbol);
    pol = rgeos_crdMat2LinearRing(env, mat, dim);
    if (n == 1) {
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, NULL,
            (unsigned int) 0)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    } else {
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(mat, R_DimSymbol);
            hole = rgeos_crdMat2LinearRing(env, mat, dim);
            holes[(j-1)] = hole;
        }
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, holes,
            (unsigned int) (n-1))) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    }
    return(res);
}

SEXP GC_Contains(SEXP env, GEOSGeom GC) {

    SEXP ans, dim;
    int pc=0;
    unsigned int i, j, n;
    int contains, ident;

    GEOSGeom Pi, Pj;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (!((int) GEOSisValid_r(GEOShandle, GC))) {
        n = (unsigned int) GEOSGetNumGeometries_r(GEOShandle, GC);
        PROTECT(ans = NEW_LIST(2)); pc++;
        PROTECT(SET_VECTOR_ELT(ans, 0, NEW_LOGICAL((int) (n*n)))); pc++;
        PROTECT(SET_VECTOR_ELT(ans, 1, NEW_LOGICAL((int) (n*n)))); pc++;
        PROTECT(dim = NEW_INTEGER(2)); pc++;
        INTEGER_POINTER(dim)[0] = (int) n;
        INTEGER_POINTER(dim)[1] = (int) n;
        setAttrib(VECTOR_ELT(ans, 0), R_DimSymbol, dim);
        setAttrib(VECTOR_ELT(ans, 1), R_DimSymbol, dim);

        for (i=0; i<n; i++) {
            if ((Pi = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC,
                 (int) i)) == NULL) {
                GEOSGeom_destroy_r(GEOShandle, GC);
                return(R_NilValue);
            } // Pi invalid
                for (j=0; j<n; j++) {
                    if ((Pj = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle,
                        GC, (int) j)) == NULL) {
                        GEOSGeom_destroy_r(GEOShandle, GC);
                        return(R_NilValue);
                    } // Pj invalid
                    if (i == j) {
                        LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = FALSE;
                        LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = FALSE;
                    } else { // i == j
                        contains = (int) GEOSContains_r(GEOShandle, Pi, Pj);
                        if (contains == 2) {
                            LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = 
                                NA_LOGICAL;
                            LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = 
                                NA_LOGICAL;
                        } else { // contains invalid
                            ident = (int) GEOSEquals_r(GEOShandle, Pi, Pj);
                            if (ident == 2) {
                                LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = 
                                    NA_LOGICAL;
                                LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = 
                                    NA_LOGICAL;
                            } else { // ident invalid
                                LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = 
                                    contains;
                                LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = 
                                  ident;
                            } // ident valid
                        } // contains valid
                    } // i != j
                } // j

        } // i
        GEOSGeom_destroy_r(GEOShandle, GC);

        UNPROTECT(pc);
        return(ans);
    } // is valid
    
    GEOSGeom_destroy_r(GEOShandle, GC);

    return(R_NilValue);
} // end of function

SEXP rgeos_PolygonsContain(SEXP env, SEXP obj) {

    GEOSGeom GC;

    GC = rgeos_Polygons2GC(env, obj);

    return(GC_Contains(env, GC));

}

SEXP rgeos_MP2crdMat(SEXP env, GEOSGeom GC) {

    int pc=0;
    unsigned int i, n;
    SEXP ans, dims, dimnames;
    GEOSGeom pt;
    double val;
    GEOSCoordSeq s;

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

            if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle,
                GC)) == NULL) {
                return(R_NilValue);
            }
            if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0,
                &val) == 0) {
                return(R_NilValue);
            }    

            NUMERIC_POINTER(ans)[0] = val;

            if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0,
                &val) == 0) {
                return(R_NilValue);
            }

            NUMERIC_POINTER(ans)[1] = val;
        

    } else {
        for (i=0; i<n; i++) {

            if ((pt = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC,
                (int) i)) == NULL) {
                return(R_NilValue);
            }
            if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle,
                pt)) == NULL) {
                return(R_NilValue);
            }
            if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) 0,
                &val) == 0) {
                return(R_NilValue);
            }    

            NUMERIC_POINTER(ans)[i] = val;

            if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) 0,
                &val) == 0) {
                return(R_NilValue);
            }

            NUMERIC_POINTER(ans)[i+n] = val;
        }
    }

    setAttrib(ans, R_DimSymbol, dims);
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_inout(SEXP env, SEXP obj) {

    SEXP ans, p4s, bb, SPans;
    GEOSGeom GC, bbG;
    int pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;

    GC = rgeos_SPoints2MP(env, obj);

    PROTECT(ans = rgeos_MP2crdMat(env, GC)); pc++;
    if (ans == R_NilValue) error("MultiPoint to matrix conversion failed");

    PROTECT(bb = rgeos_Geom2bbox(env, GC)); pc++;
    if (bb == R_NilValue) error("Bounding box creation failed");

    PROTECT(SPans = NEW_OBJECT(MAKE_CLASS("SpatialPoints"))); pc++;
    SET_SLOT(SPans, install("coords"), ans);
    SET_SLOT(SPans, install("proj4string"), p4s);
    SET_SLOT(SPans, install("bbox"), bb);

    GEOSGeom_destroy_r(GEOShandle, GC);

    UNPROTECT(pc);
    return(SPans);
}


SEXP rgeos_Geom2bbox(SEXP env, GEOSGeom Geom) {

    GEOSGeom bb, bbER;
    GEOSCoordSeq s;
    unsigned int i, n;
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
    SEXP bbmat, ans, dim, dimnames;
    int pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ((bb = GEOSEnvelope_r(GEOShandle, Geom)) == NULL) {
        return(R_NilValue);
    }

    if ((bbER = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle,
        bb)) == NULL) {
        return(R_NilValue);
    }

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle,
        bbER)) == NULL) {
        return(R_NilValue);
    }
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    if (n == 0) {
        return(R_NilValue);
    }
    
    bbmat = rgeos_CoordSeq2crdMat(env, s, (int) GEOSHasZ(bb), FALSE); 
    for (i=0; i<n; i++) {
       if (NUMERIC_POINTER(bbmat)[i] > UX) UX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] > UY) UY = NUMERIC_POINTER(bbmat)[i+n];
       if (NUMERIC_POINTER(bbmat)[i] < LX) LX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] < LY) LY = NUMERIC_POINTER(bbmat)[i+n];
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

SEXP rgeos_SpatialPolygonsSimplify(SEXP env, SEXP obj, SEXP tolerance,
    SEXP thresh) {

    double tol=NUMERIC_POINTER(tolerance)[0];
    GEOSGeom in, out;
    int pc=0, npls, i;
    SEXP ans, p4s, pls, IDs;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);
    PROTECT(IDs = NEW_CHARACTER(npls)); pc++;
    for (i=0; i<npls; i++) {
        SET_STRING_ELT(IDs, i, STRING_ELT(GET_SLOT(VECTOR_ELT(pls, i),
            install("ID")), 0));
    }

    in = rgeos_SpatialPolygonsGC(env, obj);

    if ((out = (GEOSGeometry *) GEOSTopologyPreserveSimplify_r(GEOShandle,
        in, tol)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, in);
            return(R_NilValue);
    }

    PROTECT(ans = rgeos_GCSpatialPolygons(env, out, p4s, IDs, thresh)); pc++;
    GEOSGeom_destroy(in);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_Lines_intersection(SEXP env, SEXP obj1, SEXP obj2) {

    GEOSGeom in1, in2, out;
    int pc=0, i, intersects;
    SEXP ans;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    in1 = rgeos_Lines2GC(env, obj1);
    in2 = rgeos_Lines2GC(env, obj2);

    if ((intersects = (int) GEOSIntersects_r(GEOShandle, in1, in2)) == 2) {
        error("rgeos_Lines_intersection: GEOSIntersects failure");

    }
    if (!intersects) {
        UNPROTECT(pc);
        return(R_NilValue);
    }
    if ((out = GEOSIntersection_r(GEOShandle, in1, in2)) == NULL) {
        error("rgeos_Lines_intersection: GEOSIntersection failure");
    }

    PROTECT(ans = rgeos_MP2crdMat(env, out)); pc++;
    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_Polygons_intersection(SEXP env, SEXP obj1, SEXP obj2) {

    GEOSGeom in1, in2, out;
    int pc=0, i, intersects;
    SEXP ans, ID1, ID2, thresh;
    char ibuf[BUFSIZ];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(ID1 = NEW_CHARACTER(1)); pc++;
    PROTECT(ID2 = NEW_CHARACTER(1)); pc++;
    PROTECT(thresh = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(thresh)[0] = 0.0;
    SET_STRING_ELT(ID1, 0, STRING_ELT(GET_SLOT(obj1, install("ID")), 0));
    SET_STRING_ELT(ID2, 0, STRING_ELT(GET_SLOT(obj2, install("ID")), 0));

    in1 = rgeos_Polygons2GC(env, obj1);
    in2 = rgeos_Polygons2GC(env, obj2);

    if ((intersects = (int) GEOSIntersects_r(GEOShandle, in1, in2)) == 2) {
        error("rgeos_Polygons_intersection: GEOSIntersects failure");

    }
    if (!intersects) {
        UNPROTECT(pc);
        return(R_NilValue);
    }
    if ((out = GEOSIntersection_r(GEOShandle, in1, in2)) == NULL) {
        error("rgeos_Polygons_intersection: GEOSIntersection failure");
    }

    strcpy(ibuf, CHAR(STRING_ELT(ID1, 0)));
    PROTECT(ans = rgeos_GCPolygons(env, out, ibuf, thresh)); pc++;
    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_SpatialPolygonsUnion(SEXP env, SEXP obj, SEXP grps, SEXP grpIDs,
    SEXP thresh) {

    GEOSGeom GC;
    GEOSGeom *geoms;
    int pc=0, ngrps, i;
    SEXP ans, p4s, ipls;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    ngrps = length(grps);
    geoms = (GEOSGeom *) R_alloc((size_t) ngrps, sizeof(GEOSGeom));

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(ipls = GET_SLOT(obj, install("polygons"))); pc++;

    for (i=0; i<ngrps; i++) {
        geoms[i] = rgeos_plsbufUnion(env, ipls, VECTOR_ELT(grps, i));
    }
    
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION,
        geoms, ngrps)) == NULL) {
            error("rgeos_SpatialPolygonsUnion: collection not created");
    }

    PROTECT(ans = rgeos_GCSpatialPolygons(env, GC, p4s, grpIDs, thresh)); pc++;

    UNPROTECT(pc);
    return(ans);

}

GEOSGeom rgeos_plsbufUnion(SEXP env, SEXP ipls, SEXP igrp) {

    GEOSGeom GC, iGC, oGC;
    GEOSGeom *geoms;
    int npls, i, ii;
    SEXP pl;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    npls = length(igrp);
    geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

    for (i=0; i<npls; i++) {
        ii = INTEGER_POINTER(igrp)[i] - R_OFFSET;
        pl = VECTOR_ELT(ipls, ii);
        GC = rgeos_Polygons2GC(env, pl);
        geoms[i] = GC;
    }

    if ((iGC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON,
        geoms, npls)) == NULL) {
            error("rgeos_plsbufUnion: collection not created");
    }

/* Martin Davis 100201  */
    if ((oGC = GEOSUnionCascaded_r(GEOShandle, iGC)) == NULL) {
        error("rgeos_plsbufUnion: unary union not created");
    }
    return(oGC);

}

SEXP rgeos_GCSpatialPolygons(SEXP env, GEOSGeom Geom, SEXP p4s, SEXP IDs,
    SEXP thresh) {
    SEXP ans, pls, bbox, plotOrder;
    int pc=0, ng, i;
    GEOSGeom GC, bb;
    int *po;
    double *areas;
    GEOSGeom *envs;
    char ibuf[BUFSIZ];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    ng = GEOSGetNumGeometries_r(GEOShandle, Geom);
    envs = (GEOSGeom *) R_alloc((size_t) ng, sizeof(GEOSGeom));

    PROTECT(pls = NEW_LIST(ng)); pc++;
    for (i=0; i<ng; i++) {
        GC = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, Geom, i);
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_GCSpatialPolygons bbox failure");
        }
        envs[i] = bb;
        strcpy(ibuf, CHAR(STRING_ELT(IDs, i)));
        SET_VECTOR_ELT(pls, i, rgeos_GCPolygons(env, GC, ibuf, thresh));
    }
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, envs,
        ng)) == NULL) {
        error("rgeos_GCSpatialPolygons: collection not created");
    }

    areas = (double *) R_alloc((size_t) ng, sizeof(double));
    for (i=0; i<ng; i++) 
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls, i),
            install("area")))[0]; 
    po = (int *) R_alloc((size_t) ng, sizeof(int));
    for (i=0; i<ng; i++) po[i] = i + R_OFFSET;
    revsort(areas, po, ng);


    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialPolygons"))); pc++;
    SET_SLOT(ans, install("polygons"), pls);
    SET_SLOT(ans, install("proj4string"), p4s);

    PROTECT(plotOrder = NEW_INTEGER(ng)); pc++;
    for (i=0; i<ng; i++) INTEGER_POINTER(plotOrder)[i] = po[i];
    SET_SLOT(ans, install("plotOrder"), plotOrder);

    PROTECT(bbox = rgeos_Geom2bbox(env, GC)); pc++;
    SET_SLOT(ans, install("bbox"), bbox);

    GEOSGeom_destroy_r(GEOShandle, bb);

    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_GCPolygons(SEXP env, GEOSGeom Geom, char *ibuf, SEXP thresh) {
    SEXP ans, pls, comment, Area, plotOrder, labpt, iID;
    int pc=0, ng, i, j, k, kk, nps=0, nirs;
    int *comm, *po, *idareas, *keep;
    GEOSGeom GC, lr;
    double *areas, *dareas, area;
    char buf[BUFSIZ];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (GEOSGeomTypeId_r(GEOShandle, Geom) == GEOS_POLYGON) {
        nps = GEOSGetNumInteriorRings_r(GEOShandle, Geom) + 1;
        PROTECT(pls = NEW_LIST(nps)); pc++;

        if ((GC = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, Geom))
            == NULL) error("rgeos_GCPolygons: exterior ring failure");
        comm = (int *) R_alloc((size_t) nps, sizeof(int));

        SET_VECTOR_ELT(pls, 0, rgeos_LinearRingPolygon(env, GC, FALSE));

        comm[0] = 0;

        for (i=1; i<nps; i++) {
            if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle,
                 Geom, (int) (i-1))) == NULL)
                    error("rgeos_GCPolygons: interior ring failure");
            comm[i] = 1;
            SET_VECTOR_ELT(pls, i, rgeos_LinearRingPolygon(env, lr, TRUE));
        }

    } else if (GEOSGeomTypeId_r(GEOShandle, Geom) == GEOS_MULTIPOLYGON) {

        ng = GEOSGetNumGeometries_r(GEOShandle, Geom);

        keep = (int *) R_alloc((size_t) ng, sizeof(int));
        for (i=0; i<ng; i++) keep[i] = TRUE;

        if (NUMERIC_POINTER(thresh)[0] > 0.0) {
            dareas = (double *) R_alloc((size_t) ng, sizeof(double));
            idareas = (int *) R_alloc((size_t) ng, sizeof(int));

            for (i=0; i<ng; i++) {
                GEOSArea_r(GEOShandle, (GEOSGeometry *)
                    GEOSGetGeometryN_r(GEOShandle, Geom, i), &area);
                if (area < NUMERIC_POINTER(thresh)[0]) keep[i] = FALSE;
                dareas[i] = area;
                idareas[i] = i;
            }
            for (i=0, k=0; i<ng; i++) k += keep[i];
            if (k == 0) {
                revsort(dareas, idareas, ng);
                keep[idareas[0]] = TRUE;
            }
        }

        for (i=0; i<ng; i++) {
            if (keep[i]) {
                GC = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, Geom, i);
                nps = nps + (GEOSGetNumInteriorRings_r(GEOShandle, GC) + 1);
            }
        }
        PROTECT(pls = NEW_LIST(nps)); pc++;
        comm = (int *) R_alloc((size_t) nps, sizeof(int));

        k = 0;
        for (i=0; i<ng; i++) {
            if (keep[i]) {
                GC = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, Geom, i);
                if ((lr = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle,
                    GC)) == NULL)
                    error("rgeos_GCPolygons: exterior ring failure");
                comm[k] = 0;
                kk = k + R_OFFSET;
                SET_VECTOR_ELT(pls, k, rgeos_LinearRingPolygon(env, lr, FALSE));
                k++;
                nirs = GEOSGetNumInteriorRings_r(GEOShandle, GC);
                for (j=0; j<nirs; j++) {
                    comm[k] = kk;
                    if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(
                        GEOShandle, GC, (int) (j))) == NULL)
                            error("rgeos_GCPolygons: interior ring failure");
                    SET_VECTOR_ELT(pls, k, rgeos_LinearRingPolygon(env, lr,
                        TRUE));
                    k++;
                }
            }
        }
    }

    SP_PREFIX(comm2comment)(buf, comm, nps);
    PROTECT(comment = NEW_CHARACTER(1)); pc++;
    SET_STRING_ELT(comment, 0, COPY_TO_USER_STRING(buf));

    PROTECT(iID = NEW_CHARACTER(1)); pc++;
    SET_STRING_ELT(iID, 0, COPY_TO_USER_STRING(ibuf));

    PROTECT(ans = SP_PREFIX(Polygons_c)(pls, iID)); pc++;

    setAttrib(ans, install("comment"), comment);

    GEOSGeom_destroy_r(GEOShandle, GC);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_LinearRingPolygon(SEXP env, GEOSGeom lr, int hole) {
    SEXP SPans, ans, nn, Hole, ringDir;
    double area;
    int pc=0, rev=FALSE;
    GEOSCoordSeq s;
    unsigned int n;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr))
        == NULL) error("rgeos_LinearRingPolygon: CoordSeq failure");

    rgeos_csArea(env, s, &area);
    PROTECT(ringDir = NEW_INTEGER(1)); pc++;
    PROTECT(Hole = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(Hole)[0] = hole;
    INTEGER_POINTER(ringDir)[0] = (area > 0.0) ? -1 : 1;
    if (LOGICAL_POINTER(Hole)[0] && INTEGER_POINTER(ringDir)[0] == 1) {
        rev = TRUE;
        INTEGER_POINTER(ringDir)[0] = -1;
    }
    if (!LOGICAL_POINTER(Hole)[0] && INTEGER_POINTER(ringDir)[0] == -1) {
        rev = TRUE;
        INTEGER_POINTER(ringDir)[0] = 1;
    }
    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        error("rgeos_LinearRingPolygon: CoordSeq failure");

    PROTECT(nn = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(nn)[0] = n;

    PROTECT(ans = rgeos_CoordSeq2crdMat(env, s, FALSE, rev)); pc++;

    PROTECT(SPans = SP_PREFIX(Polygon_c)(ans, nn, Hole)); pc++;

    UNPROTECT(pc);
    return(SPans);

}

GEOSGeom rgeos_SpatialPolygonsGC(SEXP env, SEXP obj) {

    SEXP pls;
    int npls,i , pc=0;
    GEOSGeom *geoms;
    GEOSGeom GC;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);

    geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

    for (i=0; i<npls; i++)
        geoms[i] = rgeos_Polygons2GC(env, VECTOR_ELT(pls, i));

    if ((GC = GEOSGeom_createCollection_r(GEOShandle,
        GEOS_GEOMETRYCOLLECTION, geoms, npls)) == NULL) {
            error("rgeos_SpatialPolygonsGC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}

