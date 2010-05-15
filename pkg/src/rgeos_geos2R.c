#include "rgeos.h"

SEXP rgeos_GCSpatialPolygons(SEXP env, GEOSGeom Geom, SEXP p4s, SEXP IDs, SEXP thresh) {
    
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
    else {
        error("rgeos_GCPolygons: Geom type id is not POLYGON or MULTIPOLYGON");
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

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr)) == NULL)
        error("rgeos_LinearRingPolygon: CoordSeq failure");

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



SEXP rgeos_multipoint2SpatialPoints(SEXP env, GEOSGeom mpt) {

    SEXP SPans, crdmat, bbox, p4s;
    int pc=0, hasZ;
    int n, type;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    type = GEOSGeomTypeId_r(GEOShandle, mpt);
    
    if ( type == GEOS_POINT ) {
        n = 1;
    } else if ( type == GEOS_MULTIPOINT ) {
        n = GEOSGetNumGeometries_r(GEOShandle, mpt);
    } else {
        error("rgeos_multipoint2SpatialPoints: unknown type");
    }
    
    if (n < 1) error("rgeos_multipoint2SpatialPoints: invalid number of geometries");

    PROTECT(crdmat = rgeos_multipoint2crdMat(env, mpt, n)); pc++;
    if (crdmat == R_NilValue) error("rgeos_multipoint2SpatialPoints: MultiPoint to matrix conversion failed");
    
    PROTECT(bbox = rgeos_crdMat2bbox(crdmat, n)); pc++;
    if (bbox == R_NilValue) error("rgeos_multipoint2SpatialPoints: bounding box creation failed");
    
    PROTECT(p4s = NEW_OBJECT(MAKE_CLASS("CRS"))); pc++;
    SET_SLOT(p4s, install("projargs"), R_NilValue); //R_NaString);

    PROTECT(SPans = NEW_OBJECT(MAKE_CLASS("SpatialPoints"))); pc++;    
    SET_SLOT(SPans, install("coords"), crdmat);
    SET_SLOT(SPans, install("bbox"), bbox);
    SET_SLOT(SPans, install("proj4string"), p4s);


    UNPROTECT(pc);
    return(SPans);
}

