#include "rgeos.h"

SEXP checkHolesGPC(SEXP env, SEXP A) {

    GEOSGeom GC = gpc2geosCollection(env, A);
    return(GC_Contains(env, GC));
}


GEOSGeom gpc2geosRing(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSCoordSeq s = gpc2geosCoordSeq(env, obj);
    GEOSGeom ring = GEOSGeom_createLinearRing_r(GEOShandle, s);
    if (ring == NULL)
        error("gpc2geosRing: error creating linear ring");
    
    if (GEOSisValid_r(GEOShandle, ring)) {
        if (GEOSNormalize_r(GEOShandle, ring) == -1)
            warning("gpc2geosRing: normalization failure");
    } else {
        warning("gpc2geosRing: validity failure");
    }

    return(ring);
}


GEOSCoordSeq gpc2geosCoordSeq(SEXP env, SEXP obj) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int n = length(VECTOR_ELT(obj, 0));
    
    // Go to n+1 so that first and last points are the same
    GEOSCoordSeq s = GEOSCoordSeq_create_r(GEOShandle, (n+1), 2);
    for(int i=0; i<(n+1); i++) {
        double x = NUMERIC_POINTER(VECTOR_ELT(obj, 0))[i % n];
        double y = NUMERIC_POINTER(VECTOR_ELT(obj, 1))[i % n];

        if (GEOSCoordSeq_setX_r(GEOShandle, s, i, x) == 0 || 
            GEOSCoordSeq_setY_r(GEOShandle, s, i, y) == 0) {
            
            error("gpc2geosCoordSeq: X or Y not set for %d", i);
        }
    }

    return(s);
}


GEOSGeom gpc2geosCollection(SEXP env, SEXP pls) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    SEXP comm;
    PROTECT(comm = SP_PREFIX(comment2comm)(pls)); pc++;
    
    int n = (comm == R_NilValue) ? length(pls) : length(comm);
    
    GEOSGeom *geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));
    
    for (int i=0; i<n; i++) {
        geoms[i] = (comm == R_NilValue)
                     ? gpc2geosPolygon(env, VECTOR_ELT(pls, i))
                     : gpc2geosPolygonHoles(env, pls, VECTOR_ELT(comm, i));
    }
    
    GEOSGeom GC = (n == 1) ? geoms[0]
        : GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, n);
    
    if (GC == NULL)
        error("gpc2geosCollection: collection not created");
    
    UNPROTECT(pc);
    return(GC);

}


GEOSGeom gpc2geosPolygon(SEXP env, SEXP obj) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    GEOSGeom shell = gpc2geosRing(env, obj);
    GEOSGeom poly = GEOSGeom_createPolygon_r(GEOShandle, shell, NULL, (unsigned int) 0);
    
    if (poly == NULL)
        error("gpc2geosPolygon: error creating polygon");

    return(poly);
}


GEOSGeom gpc2geosPolygonHoles(SEXP env, SEXP pls, SEXP vec) {

    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    int shell_index = INTEGER_POINTER(vec)[0]-R_OFFSET;
    GEOSGeom shell = gpc2geosRing(env, VECTOR_ELT(pls, shell_index));
    
    int n = length(vec);
    if (n == 0)
        error("gpc2geosPolygonHoles: polygon vector length 0");
    
    GEOSGeom *holes = NULL;
    if (n > 1) {
    
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (int j=1; j<n; j++) {
            int hole_index = INTEGER_POINTER(vec)[j]-R_OFFSET;
            holes[j-1] = gpc2geosRing(env, VECTOR_ELT(pls, hole_index));
        }
    }
    
    GEOSGeom res = GEOSGeom_createPolygon_r(GEOShandle, shell, holes, (unsigned int) (n-1));
    
    if (res == NULL)
        error("gpc2geosPolygonHoles: Error creating polygon");
    
    return(res);
}

SEXP geosPolygon2gpc(SEXP env, GEOSGeom geom) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
        
    int empty = (int) GEOSisEmpty_r(GEOShandle, geom);
    
    SEXP res;
    if (empty == 2) {
        error("geosPolygon2gpc: error evaluating geos geometry");
    } else if (empty == 1) {
        PROTECT(res = NEW_LIST(0)); pc++;
    } else {
        int type = GEOSGeomTypeId_r(GEOShandle, geom);
        
        if (type != GEOS_POLYGON && type != GEOS_MULTIPOLYGON)
            error("geosPolygon2gpc: expected polygon or multipolygon got type %d",type);
        
        int n_poly = GEOSGetNumGeometries_r(GEOShandle, geom);
        int* n_holes = (int *) R_alloc((size_t) n_poly, sizeof(int));
        
        int n_rings = 0;
        for (int i=0; i<n_poly; i++) {
            
            const GEOSGeometry *poly = GEOSGetGeometryN_r(GEOShandle, geom, i);
            
            n_holes[i] = GEOSGetNumInteriorRings_r(GEOShandle, poly);
            n_rings += n_holes[i] + 1;
        }
        
        PROTECT(res = NEW_LIST(n_rings)); pc++;
        int *comm = (int *) R_alloc((size_t) n_rings, sizeof(int));
        
        for (int i=0, j=0; i<n_poly; i++) {
            
            const GEOSGeometry *poly = GEOSGetGeometryN_r(GEOShandle, geom, i);
            
            GEOSGeom shell = (GEOSGeom) GEOSGetExteriorRing_r(GEOShandle, poly);
            if (shell == NULL) 
                error("geosPolygon2gpc: exterior ring failure");

            comm[j] = 0;
            SET_VECTOR_ELT(res, j, geosRing2gpc(env, shell, FALSE));
            int cur_index = j;
            j++;
        
            for (int k=0; k<n_holes[i]; k++, j++) {
                GEOSGeom hole = (GEOSGeom) GEOSGetInteriorRingN_r(GEOShandle, poly, (int) k);
                if (hole == NULL)
                     error("geosPolygon2gpc: interior ring failure");
                comm[j] = cur_index;
                SET_VECTOR_ELT(res, j, geosRing2gpc(env, hole, TRUE));
            }
        }
        
        int nchar = ceil(log10(n_rings)+1)+1;
        char *buf = (char *) R_alloc((size_t) (n_rings*nchar)+1, sizeof(char));
        SP_PREFIX(comm2comment)(buf, (n_rings*nchar)+1, comm, n_rings);
        
        SEXP comment;
        PROTECT(comment = NEW_CHARACTER(1)); pc++;
        SET_STRING_ELT(comment, 0, mkChar((const char*) buf));
        
        setAttrib(res, install("comment"), comment);
    }

    UNPROTECT(pc);
    return(res);
}

SEXP geosRing2gpc(SEXP env, GEOSGeom lr, int hole) {
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    int pc=0;
    
    GEOSCoordSeq s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, lr);
    
    if (s == NULL)
        error("geosRing2gpc: CoordSeq failure");
    
    unsigned int n;
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    
    SEXP res;
    PROTECT(res = NEW_LIST(3)); pc++;
    SET_VECTOR_ELT(res, 0, NEW_NUMERIC(n));
    SET_VECTOR_ELT(res, 1, NEW_NUMERIC(n));
    SET_VECTOR_ELT(res, 2, NEW_LOGICAL(1));
    LOGICAL_POINTER(VECTOR_ELT(res, 2))[0] = hole;
    
    SEXP nms;
    PROTECT(nms = NEW_CHARACTER(3)); pc++;
    SET_STRING_ELT(nms, 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(nms, 1, COPY_TO_USER_STRING("y"));
    SET_STRING_ELT(nms, 2, COPY_TO_USER_STRING("hole"));
    
    setAttrib(res, R_NamesSymbol, nms);
    
    for (int i=0; i<n; i++) {
        GEOSCoordSeq_getX_r(GEOShandle, s, i, NUMERIC_POINTER(VECTOR_ELT(res, 0))+i);
        GEOSCoordSeq_getY_r(GEOShandle, s, i, NUMERIC_POINTER(VECTOR_ELT(res, 1))+i);
    }
    
    //GEOSCoordSeq_destroy_r(GEOShandle,s); 
    
    UNPROTECT(pc);
    return(res);
}


