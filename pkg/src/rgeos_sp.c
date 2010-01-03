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

    PROTECT(comm = SP_PREFIX(comment2comm)(obj)); pc++;

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
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
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
    
    bbmat = rgeos_CoordSeq2crdMat(s, (int) GEOSHasZ(bb), FALSE); 
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

SEXP rgeos_SpatialPolygonsSimplify(SEXP obj, SEXP tolerance, SEXP thresh) {

    double tol=NUMERIC_POINTER(tolerance)[0];
    GEOSGeom in, out;
    int pc=0, npls, i;
    SEXP ans, p4s, pls, IDs;

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);
    PROTECT(IDs = NEW_CHARACTER(npls)); pc++;
    for (i=0; i<npls; i++) {
        SET_STRING_ELT(IDs, i, STRING_ELT(GET_SLOT(VECTOR_ELT(pls, i),
            install("ID")), 0));
    }

    in = rgeos_SpatialPolygonsGC(obj);

    if ((out = (GEOSGeometry *) GEOSTopologyPreserveSimplify(in, tol))
        == NULL) {
            GEOSGeom_destroy(in);
            return(R_NilValue);
    }

    PROTECT(ans = rgeos_GCSpatialPolygons(out, p4s, IDs, thresh)); pc++;
    GEOSGeom_destroy(in);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_SpatialPolygonsUnion(SEXP obj, SEXP grps, SEXP grpIDs, SEXP thresh) {

    GEOSGeom GC;
    GEOSGeom *geoms;
    int pc=0, ngrps, i;
    SEXP ans, p4s, ipls;

    ngrps = length(grps);
    geoms = (GEOSGeom *) R_alloc((size_t) ngrps, sizeof(GEOSGeom));

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(ipls = GET_SLOT(obj, install("polygons"))); pc++;

    for (i=0; i<ngrps; i++) {
        GC = rgeos_plsUnion(ipls, VECTOR_ELT(grps, i));
        geoms[i] = GC;
    }
    
    if ((GC = GEOSGeom_createCollection(GEOS_GEOMETRYCOLLECTION, geoms,
        ngrps)) == NULL) {
            error("rgeos_SpatialPolygonsUnion: collection not created");
    }

    PROTECT(ans = rgeos_GCSpatialPolygons(GC, p4s, grpIDs, thresh)); pc++;

    UNPROTECT(pc);
    return(ans);

}

GEOSGeom rgeos_plsUnion(SEXP ipls, SEXP igrp) {

    GEOSGeom GC, iGC, oGC;
    GEOSGeom *geoms, *ggeoms;
    int npls, i, ii, j, nnpls;
    int *ngeoms;
    SEXP pl;

    npls = length(igrp);
    geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    ngeoms = (int *) R_alloc((size_t) npls, sizeof(int));

    for (i=0, nnpls=0; i<npls; i++) {
        ii = INTEGER_POINTER(igrp)[i] - R_OFFSET;
        pl = VECTOR_ELT(ipls, ii);
        GC = rgeos_Polygons2GC(pl);
        geoms[i] = GC;
        ngeoms[i] = GEOSGetNumGeometries(GC);
        nnpls += ngeoms[i];
    }

    ggeoms = (GEOSGeom *) R_alloc((size_t) nnpls, sizeof(GEOSGeom));
    for(i=0, ii=0; i<npls; i++) {
        GC = geoms[i];
        for (j=0; j<ngeoms[i]; j++) {
            ggeoms[ii] = (GEOSGeometry *) GEOSGetGeometryN(GC, j);
            ii++;
        }
    }
    
    if ((iGC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, ggeoms,
        nnpls)) == NULL) {
            error("rgeos_plsUnion: collection not created");
    }
    if ((oGC = GEOSBuffer(iGC, 0.0, 100)) == NULL) {
            error("rgeos_plsUnion: buffer not created");
    }

    return(oGC);

}

SEXP rgeos_GCSpatialPolygons(GEOSGeom Geom, SEXP p4s, SEXP IDs, SEXP thresh) {
    SEXP ans, pls, bbox, plotOrder;
    int pc=0, ng, i;
    GEOSGeom GC, bb;
    int *po;
    double *areas;
    GEOSGeom *envs;
    char ibuf[BUFSIZ];

    ng = GEOSGetNumGeometries(Geom);
    envs = (GEOSGeom *) R_alloc((size_t) ng, sizeof(GEOSGeom));

    PROTECT(pls = NEW_LIST(ng)); pc++;
    for (i=0; i<ng; i++) {
        GC = (GEOSGeometry *) GEOSGetGeometryN(Geom, i);
        if ((bb = GEOSEnvelope(GC)) == NULL) {
            error("rgeos_GCSpatialPolygons bbox failure");
        }
        envs[i] = bb;
        strcpy(ibuf, CHAR(STRING_ELT(IDs, i)));
        SET_VECTOR_ELT(pls, i, rgeos_GCPolygons(GC, ibuf, thresh));
    }
    if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, envs,
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

    PROTECT(bbox = rgeos_Geom2bbox(GC)); pc++;
    SET_SLOT(ans, install("bbox"), bbox);

    GEOSGeom_destroy(bb);

    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_GCPolygons(GEOSGeom Geom, char *ibuf, SEXP thresh) {
    SEXP ans, pls, comment, Area, plotOrder, labpt, iID;
    int pc=0, ng, i, j, k, kk, nps=0, nirs;
    int *comm, *po, *idareas, *keep;
    GEOSGeom GC, lr;
    double *areas, *dareas, area;
    char buf[BUFSIZ];


    if (GEOSGeomTypeId(Geom) == GEOS_POLYGON) {
        nps = GEOSGetNumInteriorRings(Geom) + 1;
        PROTECT(pls = NEW_LIST(nps)); pc++;

        if ((GC = (GEOSGeometry *) GEOSGetExteriorRing(Geom)) == NULL)
            error("rgeos_GCPolygons: exterior ring failure");
        comm = (int *) R_alloc((size_t) nps, sizeof(int));

        SET_VECTOR_ELT(pls, 0, rgeos_LinearRingPolygon(GC, FALSE));

        comm[0] = 0;

        for (i=1; i<nps; i++) {
            if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN(Geom, (int) (i-1)))
                 == NULL)
                    error("rgeos_GCPolygons: interior ring failure");
            comm[i] = 1;
            SET_VECTOR_ELT(pls, i, rgeos_LinearRingPolygon(lr, TRUE));
        }

    } else if (GEOSGeomTypeId(Geom) == GEOS_MULTIPOLYGON) {

        ng = GEOSGetNumGeometries(Geom);

        keep = (int *) R_alloc((size_t) ng, sizeof(int));
        for (i=0; i<ng; i++) keep[i] = TRUE;

        if (NUMERIC_POINTER(thresh)[0] > 0.0) {
            dareas = (double *) R_alloc((size_t) ng, sizeof(double));
            idareas = (int *) R_alloc((size_t) ng, sizeof(int));

            for (i=0; i<ng; i++) {
                GEOSArea((GEOSGeometry *) GEOSGetGeometryN(Geom, i), &area);
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
                GC = (GEOSGeometry *) GEOSGetGeometryN(Geom, i);
                nps = nps + (GEOSGetNumInteriorRings(GC) + 1);
            }
        }
        PROTECT(pls = NEW_LIST(nps)); pc++;
        comm = (int *) R_alloc((size_t) nps, sizeof(int));

        k = 0;
        for (i=0; i<ng; i++) {
            if (keep[i]) {
                GC = (GEOSGeometry *) GEOSGetGeometryN(Geom, i);
                if ((lr = (GEOSGeometry *) GEOSGetExteriorRing(GC)) == NULL)
                    error("rgeos_GCPolygons: exterior ring failure");
                comm[k] = 0;
                kk = k + R_OFFSET;
                SET_VECTOR_ELT(pls, k, rgeos_LinearRingPolygon(lr, FALSE));
                k++;
                nirs = GEOSGetNumInteriorRings(GC);
                for (j=0; j<nirs; j++) {
                    comm[k] = kk;
                    if ((lr = (GEOSGeometry *) GEOSGetInteriorRingN(GC, 
                        (int) (j))) == NULL)
                            error("rgeos_GCPolygons: interior ring failure");
                    SET_VECTOR_ELT(pls, k, rgeos_LinearRingPolygon(lr, TRUE));
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

    GEOSGeom_destroy(GC);
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_LinearRingPolygon(GEOSGeom lr, int hole) {
    SEXP SPans, ans, nn, Hole, ringDir;
    double area;
    int pc=0, rev=FALSE;
    GEOSCoordSeq s;
    unsigned int n;


    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq(lr)) == NULL)
        error("rgeos_LinearRingPolygon: CoordSeq failure");

    rgeos_csArea(s, &area);
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
    if (GEOSCoordSeq_getSize(s, &n) == 0)
        error("rgeos_LinearRingPolygon: CoordSeq failure");

    PROTECT(nn = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(nn)[0] = n;

    PROTECT(ans = rgeos_CoordSeq2crdMat(s, FALSE, rev)); pc++;

    PROTECT(SPans = SP_PREFIX(Polygon_c)(ans, nn, Hole)); pc++;

    UNPROTECT(pc);
    return(SPans);

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

