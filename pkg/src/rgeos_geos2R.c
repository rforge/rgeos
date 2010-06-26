#include "rgeos.h"

SEXP rgeos_convert_geos2R(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, SEXP thres) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    int ng = GEOSGetNumGeometries_r(GEOShandle, geom);
    if (ng == -1) error("rgeos_convert_geos2R: invalid number of subgeometries"); 
    ng = ng ? ng : 1; // Empty MULTI type geometries return size 0
    
    int pc=0;
    SEXP ans;
    switch(type) { // Determine appropriate conversion for the collection
        case -1:
            error("rgeos_convert_geos2R: unknown geometry type");
            break;
            
        case GEOS_POINT:
        case GEOS_MULTIPOINT:
            PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, ng) ); pc++;
            break;
    
        case GEOS_LINEARRING:
            PROTECT( ans = rgeos_geosring2SpatialRings(env, geom, p4s, id, ng)); pc++;
            break;
            
        case GEOS_LINESTRING:
        case GEOS_MULTILINESTRING:
            PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, 1) ); pc++;
            break;
    
        case GEOS_POLYGON:
        case GEOS_MULTIPOLYGON:
            PROTECT( ans = rgeos_geospolygon2SpatialPolygons(env, geom,p4s, id, 1, thres) ); pc++;
            break;
        
        case GEOS_GEOMETRYCOLLECTION:
        {    
            int gctypes[] = {0,0,0,0,0,0,0,0};
            int n=0;
            
            for (int i=0; i<ng; i++) {
                GEOSGeom subgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
                if (subgeom == NULL)
                    error("rgeos_convert_geos2R: unable to retrieve subgeometry");
                
                int ns = GEOSGetNumGeometries_r(GEOShandle, subgeom);
                if (ns == -1) error("rgeos_convert_geos2R: invalid number of geometries in subgeometry");
                ns = ns ? ns : 1;
                n += ns;
                
                gctypes[ GEOSGeomTypeId_r(GEOShandle, subgeom) ] += 1; 
            }
            
            int isPoint = gctypes[GEOS_POINT] || gctypes[GEOS_MULTIPOINT];
            int isLine  = gctypes[GEOS_LINESTRING] || gctypes[GEOS_MULTILINESTRING];
            int isPoly  = gctypes[GEOS_POLYGON] || gctypes[GEOS_MULTIPOLYGON];
            int isRing  = gctypes[GEOS_LINEARRING];
            int isGC    = gctypes[GEOS_GEOMETRYCOLLECTION];
            
            //Rprintf("isPoint: %d  isLine: %d  isPoly: %d  isRing: %d  isGC: %d\n",isPoint, isLine, isPoly, isRing, isGC);
            
            if ( isGC ) {
                error("Geometry collections may not contain other geometry collections");
            }
            
            if ( isPoint && !isLine && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geospoint2SpatialPoints(env, geom, p4s, id, n) ); pc++;
            } else if ( isLine && !isPoint && !isPoly && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geosline2SpatialLines(env, geom, p4s, id, ng) ); pc++;
            } else if ( isPoly && !isPoint && !isLine && !isRing && !isGC ) {
                PROTECT( ans = rgeos_geospolygon2SpatialPolygons(env, geom, p4s,id, ng, thres) ); pc++;
            } else if ( isRing && !isPoint && !isLine && !isPoly && !isGC ) {
                PROTECT( ans = rgeos_geosring2SpatialRings(env, geom, p4s, id, ng) ); pc++;    
            } else {
                error("Geometry Collection Invalid - heterogenous collections cannot be processed");
            }
            
            break;
        }    
        default:
            error("Unknown type");
    }

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geospolygon2SpatialPolygons(SEXP env, GEOSGeom geom, SEXP p4s, SEXP IDs, int ng, SEXP thresh) {
    
    SEXP ans, poly, pls, bbox, plotOrder;
    int pc=0, i, type;
    GEOSGeom GC;
    int *po;
    double *areas;
    char ibuf[BUFSIZ];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    
    type = GEOSGeomTypeId_r(GEOShandle, geom);
    
    if (ng < 1) error("rgeos_geospolygon2SpatialPolygons: invalid number of geometries");
    
    PROTECT(pls = NEW_LIST(ng)); pc++;
    areas = (double *) R_alloc((size_t) ng, sizeof(double));
    po = (int *) R_alloc((size_t) ng, sizeof(int));
    
    GC = geom;
    for (i=0; i<ng; i++) {
        if (type == GEOS_GEOMETRYCOLLECTION) {
            GC = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, geom, i);
        }
        if (GC == NULL) error("rgeos_geospolygon2SpatialPolygons: unable to get subgeometry");
        
        strcpy(ibuf, CHAR(STRING_ELT(IDs, i)));
        PROTECT( poly = rgeos_GCPolygons(env, GC, ibuf, thresh) );
        
        areas[i] = NUMERIC_POINTER(GET_SLOT(poly, install("area")))[0];
        SET_VECTOR_ELT(pls, i, poly);
        
        po[i] = i + R_OFFSET;
        
        UNPROTECT(1); 
    }
    
    revsort(areas, po, ng);
    
    PROTECT(plotOrder = NEW_INTEGER(ng)); pc++;
    for (i=0; i<ng; i++) INTEGER_POINTER(plotOrder)[i] = po[i];
    

    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialPolygons"))); pc++;
    SET_SLOT(ans, install("polygons"), pls);
    SET_SLOT(ans, install("proj4string"), p4s);
    SET_SLOT(ans, install("plotOrder"), plotOrder);
    SET_SLOT(ans, install("bbox"), bbox);

    UNPROTECT(pc);
    return(ans);

}

SEXP rgeos_GCPolygons(SEXP env, GEOSGeom Geom, char *ibuf, SEXP thresh) {
    SEXP ans, pls, comment, Area, plotOrder, labpt, iID;
    int pc=0, ng, i, j, k, kk, nps=0, nirs, type;
    int *comm, *po, *idareas, *keep;
    GEOSGeom GC, lr;
    double *areas, *dareas, area;
    char buf[BUFSIZ];

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    type = GEOSGeomTypeId_r(GEOShandle, Geom);
    
    if (type == GEOS_POLYGON) {
        
        nps = GEOSGetNumInteriorRings_r(GEOShandle, Geom) + 1;
        PROTECT(pls = NEW_LIST(nps)); pc++;

        GC = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, Geom);
        if (GC == NULL) error("rgeos_GCPolygons: exterior ring failure");
        
        comm = (int *) R_alloc((size_t) nps, sizeof(int));

        SET_VECTOR_ELT(pls, 0, rgeos_LinearRingPolygon(env, GC, FALSE));

        comm[0] = 0;

        for (i=1; i<nps; i++) {
            lr = (GEOSGeometry *) GEOSGetInteriorRingN_r(GEOShandle, Geom, (int) (i-1));
            if (lr == NULL) error("rgeos_GCPolygons: interior ring failure");
            
            comm[i] = 1;
            SET_VECTOR_ELT(pls, i, rgeos_LinearRingPolygon(env, lr, TRUE));
        }

    } else if (type == GEOS_MULTIPOLYGON) {

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
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom p = GEOSGeom_createPolygon_r(GEOShandle,lr,NULL,0);
    if (p == NULL) error("rgeos_LinearRingPolygon: unable to create polygon");
    
    double area;
    if (!GEOSArea_r(GEOShandle, p, &area)) {
        error("rgeos_LinearRingPolygon: area calculation failure");
    }

    GEOSCoordSeq s;
    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr)) == NULL)
        error("rgeos_LinearRingPolygon: CoordSeq failure");

    int pc=0;
    SEXP Hole, ringDir;
    PROTECT(ringDir = NEW_INTEGER(1)); pc++;
    PROTECT(Hole = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(Hole)[0] = hole;
    INTEGER_POINTER(ringDir)[0] = (area > 0.0) ? -1 : 1;
    
    int rev=FALSE;
    if (LOGICAL_POINTER(Hole)[0] && INTEGER_POINTER(ringDir)[0] == 1) {
        rev = TRUE;
        INTEGER_POINTER(ringDir)[0] = -1;
    }
    if (!LOGICAL_POINTER(Hole)[0] && INTEGER_POINTER(ringDir)[0] == -1) {
        rev = TRUE;
        INTEGER_POINTER(ringDir)[0] = 1;
    }
    
    unsigned int n;
    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        error("rgeos_LinearRingPolygon: CoordSeq failure");
    
    SEXP nn;
    PROTECT(nn = NEW_INTEGER(1)); pc++;
    INTEGER_POINTER(nn)[0] = n;

    SEXP crd, ans;
    PROTECT(crd = rgeos_CoordSeq2crdMat(env, s, FALSE, rev)); pc++;
    PROTECT(ans = SP_PREFIX(Polygon_c)(crd, nn, Hole)); pc++;
    
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_geospoint2SpatialPoints(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int n) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
        
    int type = GEOSGeomTypeId_r(GEOShandle, geom);    
    if ( type != GEOS_POINT && type != GEOS_MULTIPOINT && type != GEOS_GEOMETRYCOLLECTION )
        error("rgeos_geospoint2SpatialPoints: invalid geometry type");
    
    int pc=0;
    SEXP bbox, crdmat;
    
    //if (GEOSisEmpty_r(GEOShandle, geom)==0) {
        PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
        PROTECT(crdmat = rgeos_geospoint2crdMat(env, geom, id, n, type)); pc++;
    //} else {
    //    bbox = R_NilValue;
    //    crdmat = R_NilValue;
    //}
    
    SEXP ans;
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialPoints"))); pc++;    
    SET_SLOT(ans, install("coords"), crdmat);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geosline2SpatialLines(SEXP env, GEOSGeom geom, SEXP p4s, SEXP idlist, int nlines) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    if (type != GEOS_LINESTRING && type != GEOS_MULTILINESTRING && 
        type != GEOS_LINEARRING && type != GEOS_GEOMETRYCOLLECTION ) {

        error("rgeos_geosline2SpatialLines: invalid type");
    }
    if (nlines < 1) error("rgeos_geosline2SpatialLines: invalid number of geometries");
    
    int pc=0;
    SEXP bbox, lines_list;
    PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    PROTECT(lines_list = NEW_LIST(nlines)); pc++;
    
    for(int j = 0; j < nlines; j++) {
        
        GEOSGeom curgeom = (type == GEOS_GEOMETRYCOLLECTION) ?
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j) :
                                geom;
        if (curgeom == NULL) 
            error("rgeos_geosline2SpatialLines: unable to get geometry collection geometry");
        int curtype = GEOSGeomTypeId_r(GEOShandle, curgeom);
        
        int n = GEOSGetNumGeometries_r(GEOShandle, curgeom);
        if (n == -1) error("rgeos_geosline2SpatialLines: invalid number of geometries in current geometry");
        n = n ? n : 1;
        
        SEXP line_list;
        PROTECT(line_list = NEW_LIST(n));
        
        for(int i = 0; i < n; i++) {
            GEOSGeom subgeom = (curtype == GEOS_MULTILINESTRING && !GEOSisEmpty_r(GEOShandle, curgeom)) ?
                                    (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, curgeom, i) :
                                    curgeom;
            if(subgeom == NULL) error("rgeos_geosline2SpatialLines: unable to get subgeometry");
            
            SEXP crdmat;
            if (GEOSisEmpty_r(GEOShandle, subgeom) == 0) {
                GEOSCoordSeq s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, subgeom);
                if (s == NULL) 
                    error("rgeos_geosline2SpatialLines: unable to generate coordinate sequence");

                PROTECT( crdmat = rgeos_CoordSeq2crdMat(env, s, FALSE, FALSE));
                //GEOSCoordSeq_destroy_r(GEOShandle, s);
            } else {
                PROTECT( crdmat = R_NilValue);
            }
            SEXP line;
            PROTECT(line = NEW_OBJECT(MAKE_CLASS("Line")));   
            SET_SLOT(line, install("coords"), crdmat);
            SET_VECTOR_ELT(line_list, i, line );
        
            UNPROTECT(2);
        }
        SEXP lines;
        PROTECT( lines = NEW_OBJECT(MAKE_CLASS("Lines")) );
        SET_SLOT(lines, install("Lines"), line_list);
        
        char idbuf[BUFSIZ];
        strcpy(idbuf, CHAR( STRING_ELT(idlist, j) ));
        
        SEXP id;
        PROTECT( id = NEW_CHARACTER(1) );
        SET_STRING_ELT(id, 0, COPY_TO_USER_STRING(idbuf));
        SET_SLOT(lines, install("ID"), id);

        SET_VECTOR_ELT( lines_list, j, lines );
        
        UNPROTECT(3);
    }
    
    SEXP ans;    
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialLines"))); pc++;    
    SET_SLOT(ans, install("lines"), lines_list);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);

    UNPROTECT(pc);
    return(ans);
}


SEXP rgeos_geosring2SpatialRings(SEXP env, GEOSGeom geom, SEXP p4s, SEXP idlist, int nrings) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int type = GEOSGeomTypeId_r(GEOShandle, geom);
    if (type != GEOS_LINEARRING && type != GEOS_GEOMETRYCOLLECTION )
        error("rgeos_geosring2SpatialRings: invalid type");
    
    if (nrings < 1) error("rgeos_geosring2SpatialRings: invalid number of geometries");
    
    int pc=0;
    SEXP bbox, rings_list;
    PROTECT(bbox = rgeos_geom2bbox(env, geom)); pc++;
    PROTECT(rings_list = NEW_LIST(nrings)); pc++;
    
    for(int j = 0; j < nrings; j++) {
        
        GEOSGeom curgeom = (type == GEOS_GEOMETRYCOLLECTION) ?
                                (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, j) :
                                geom;
        if (curgeom == NULL) 
            error("rgeos_geosring2SpatialRings: unable to get geometry collection geometry");
        
        SEXP crdmat;
        if (GEOSisEmpty_r(GEOShandle, curgeom) == 0) {
            GEOSCoordSeq s = (GEOSCoordSeq) GEOSGeom_getCoordSeq_r(GEOShandle, curgeom);
            if (s == NULL) 
                error("rgeos_geosring2SpatialRings: unable to generate coordinate sequence");

            PROTECT( crdmat = rgeos_CoordSeq2crdMat(env, s, FALSE, FALSE));
            //GEOSCoordSeq_destroy_r(GEOShandle, s);
        } else {
            PROTECT( crdmat = R_NilValue);
        }
        
        SEXP ring;
        PROTECT(ring = NEW_OBJECT(MAKE_CLASS("Ring")));   
        SET_SLOT(ring, install("coords"), crdmat);
        
        SEXP id;
        PROTECT( id = NEW_CHARACTER(1) );
        char idbuf[BUFSIZ];
        strcpy(idbuf, CHAR( STRING_ELT(idlist, j) ));
        SET_STRING_ELT(id, 0, COPY_TO_USER_STRING(idbuf));
        
        SET_SLOT(ring, install("ID"), id);

        SET_VECTOR_ELT(rings_list, j, ring );
        
        
        UNPROTECT(3);
    }
    
    SEXP ans;    
    PROTECT(ans = NEW_OBJECT(MAKE_CLASS("SpatialRings"))); pc++;    
    SET_SLOT(ans, install("rings"), rings_list);
    SET_SLOT(ans, install("bbox"), bbox);
    SET_SLOT(ans, install("proj4string"), p4s);

    UNPROTECT(pc);
    return(ans);
}