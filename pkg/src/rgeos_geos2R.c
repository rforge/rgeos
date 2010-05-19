#include "rgeos.h"

SEXP rgeos_convert_geos2R(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, SEXP thres) {
    
    SEXP ans;
    GEOSGeom subgeom;
    int type, i,pc=0;
    int ng, ns, n=0;
    int gctypes[] = {0,0,0,0,0,0,0,0};
    int isPoint, isLine, isPoly, isRing, isGC;
    
    char ibuf[BUFSIZ];
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    strcpy(ibuf, CHAR(STRING_ELT(id, 0)));

    type = GEOSGeomTypeId_r(GEOShandle, geom);
    ng = GEOSGetNumGeometries_r(GEOShandle, geom);
    if (ng == -1) error("rgeos_convert_geos2R: invalid number of subgeometries"); 
    
    switch(type) { // Determine appropriate type for the collection
        case -1:
            error("rgeos_convert_geos2R: unknown geometry type");
            break;
            
        case GEOS_POINT:
        case GEOS_MULTIPOINT:
            PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, ng) ); pc++;
            break;
    
        case GEOS_LINEARRING:
        case GEOS_LINESTRING:
        case GEOS_MULTILINESTRING:
            PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, ng) ); pc++;
            break;
    
        case GEOS_POLYGON:
            PROTECT( ans = rgeos_GCPolygons(env, geom, ibuf, thres) ); pc++;
            break;
        
        case GEOS_GEOMETRYCOLLECTION:
            
            for (i=0; i<ng; i++) {
                if ((subgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i)) == NULL)
                    error("rgeos_convert_geos2R: unable to retrieve subgeometry");
                
                type = GEOSGeomTypeId_r(GEOShandle, subgeom);
                ns = GEOSGetNumGeometries_r(GEOShandle, subgeom);
                if(ns == -1) error("rgeos_convert_geos2R: invalid number of geometries in subgeometry");
                n += ns;
                
                gctypes[type] = TRUE; 
            }
            isPoint = gctypes[GEOS_POINT] || gctypes[GEOS_MULTIPOINT];
            isLine  = gctypes[GEOS_LINESTRING] || gctypes[GEOS_MULTILINESTRING];
            isPoly  = gctypes[GEOS_POLYGON] || gctypes[GEOS_MULTIPOLYGON];
            isRing  = gctypes[GEOS_LINEARRING];
            isGC    = gctypes[GEOS_GEOMETRYCOLLECTION];
            
            Rprintf("isPoint: %d  isLine: %d  isPoly: %d  isRing: %d  isGC: %d\n",isPoint, isLine, isPoly, isRing, isGC);
            
            if ( isPoint && !isLine && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, n) ); pc++;
            /*} else if ( !isPoint && isLine && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, ng) ); pc++;
            } else if ( !isPoint && !isLine && isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_GCPolygons(env, geom, ibuf, thres) ); pc++;
            } else if ( !isPoint && !isLine && !isPoly && isRing && !isGC ) {
                PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, ng) ); pc++;
            */    
            } else {
                error("Geometry Collection Invalid - heterogenous collections cannot be processed");
            }
            
            break;
            
        case GEOS_MULTIPOLYGON:
            error("Sorry I don't know how to handle this type yet");
            //PROTECT( ans = R_NilValue ); pc++;
    }


    UNPROTECT(pc);
    return(ans);
}


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
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, envs, ng)) == NULL) {
        error("rgeos_GCSpatialPolygons: collection not created");
    }

    areas = (double *) R_alloc((size_t) ng, sizeof(double));
    for (i=0; i<ng; i++) 
        areas[i] = NUMERIC_POINTER(GET_SLOT(VECTOR_ELT(pls, i), install("area")))[0]; 
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

SEXP rgeos_geospoint2SpatialPoints(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int n) {

    SEXP ans, crdmat, bbox;
    int pc=0;
    int i, type;
    GEOSGeom subgeom;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    if ( type != GEOS_POINT && type != GEOS_MULTIPOINT && type != GEOS_GEOMETRYCOLLECTION )
        error("rgeos_geospoint2SpatialPoints: invalid geometry type");
    
    if (n < 1) error("rgeos_geospoint2SpatialPoints: invalid number of geometries");

    PROTECT(crdmat = rgeos_geospoint2crdMat(env, geom, n, type)); pc++;
    if (crdmat == R_NilValue) error("rgeos_geospoint2SpatialPoints: geos point to coord matrix conversion failed");
    
    PROTECT(bbox = rgeos_initbbox()); pc++;
    rgeos_updatebbox_crdmat(bbox, crdmat, n);
    PROTECT(bbox = rgeos_formatbbox(bbox)); pc++;

    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialPoints"))); pc++;    
    SET_SLOT(ans, install("coords"), crdmat);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);


    UNPROTECT(pc);
    return(ans);
}




SEXP rgeos_geosline2SpatialLines(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int ng) {

    SEXP line, line_list, lines, lines_list, ans, crdmat, bbox;
    int pc=0;
    int i, n, type, hasZ,ncoord;
    GEOSGeom curgeom;
    GEOSCoordSeq s;
    double val,xs,ys,xe,ye,scale = getScale(env);
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    if ( type == GEOS_LINESTRING || type == GEOS_LINEARRING ) {
        n = 1;
    } else if ( type == GEOS_MULTILINESTRING ) {
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    } else {
        error("rgeos_geosline2SpatialLines: unknown type");
    }
    if (n < 1) error("rgeos_geosline2SpatialLines: invalid number of geometries");

    PROTECT(line_list = NEW_LIST(n)); pc++;
    PROTECT(bbox = rgeos_initbbox()); pc++;
    
    if ( n==1 ) curgeom = geom;
    for(i = 0; i < n; i++) {
        if( n != 1) {
            if ((curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i)) == NULL)
                error("rgeos_geosline2SpatialLines: unable to get GetGeometryN");
        }
    
        hasZ = (int) GEOSHasZ_r(GEOShandle, curgeom);
        if ((s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, curgeom)) == NULL)
            error("rgeos_geosline2SpatialLines: unable to generate coordinate sequence");
        
        if (GEOSCoordSeq_getSize_r(GEOShandle, s, &ncoord) == 0)
            error("rgeos_geosline2SpatialLines: unable to determine length of coordinate sequence");
                
        PROTECT( crdmat = rgeos_CoordSeq2crdMat(env, s, hasZ, FALSE)); //pc++;
        if (crdmat == R_NilValue) error("rgeos_geosline2SpatialLines: CoordSeq to crdMat conversion failed");
        rgeos_updatebbox_crdmat(bbox, crdmat, ncoord);

        PROTECT(line = NEW_OBJECT(MAKE_CLASS("Line"))); //pc++;    
        SET_SLOT(line, install("coords"), crdmat);
        SET_VECTOR_ELT(line_list, i, line );
        
        GEOSCoordSeq_destroy_r(GEOShandle, s);
        UNPROTECT(2);
    }
    PROTECT(bbox = rgeos_formatbbox(bbox)); pc++;
    
    
    PROTECT(lines = NEW_OBJECT(MAKE_CLASS("Lines"))); pc++;
    SET_SLOT(lines, install("Lines"), line_list); //R_NaString);
    SET_SLOT(lines, install("ID"), id);
    
    PROTECT(lines_list = NEW_LIST(1)); pc++;
    SET_VECTOR_ELT(lines_list, 0, lines );
    
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialLines"))); pc++;    
    SET_SLOT(ans, install("lines"), lines_list);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);


    UNPROTECT(pc);
    return(ans);
}
