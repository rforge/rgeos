#include "rgeos.h"


SEXP rgeos_SpatialPolygonsSimplify(SEXP env, SEXP obj, SEXP tolerance, SEXP thresh) {

    double tol = NUMERIC_POINTER(tolerance)[0];
    GEOSGeom in, out;
    int pc=0, npls, i;
    SEXP ans, p4s, pls, IDs;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(p4s = GET_SLOT(obj, install("proj4string"))); pc++;
    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);
    PROTECT(IDs = NEW_CHARACTER(npls)); pc++;
    for (i=0; i<npls; i++) {
        SET_STRING_ELT(IDs, i, STRING_ELT(GET_SLOT(VECTOR_ELT(pls, i), install("ID")), 0));
    }

    in = rgeos_SpatialPolygons2geospolygon(env, obj);

    if ((out = (GEOSGeometry *) GEOSTopologyPreserveSimplify_r(GEOShandle, in, tol)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, in);
            return(R_NilValue);
    }

    PROTECT(ans = rgeos_geospolygon2SpatialPolygons(env, out, p4s, IDs, 1, thresh)); pc++;
    GEOSGeom_destroy(in);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_Lines_intersection(SEXP env, SEXP obj1, SEXP obj2) {

    GEOSGeom in1, in2, out;
    int pc=0, i, intersects, n;
    SEXP ans,p4s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    in1 = rgeos_Lines2geosline(env, obj1);
    in2 = rgeos_Lines2geosline(env, obj2);

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
    
    // FIXME
    n = GEOSGetNumGeometries_r(GEOShandle, out);
    if (n == -1) error("rgeos_Lines_intersection: invalid number of subgeometries"); 

    PROTECT(p4s = GET_SLOT(obj1, install("proj4string"))); pc++;
    
    PROTECT(ans = rgeos_geospoint2SpatialPoints(env, out, p4s, R_NilValue, n)); pc++;
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

    in1 = rgeos_Polygons2geospolygon(env, obj1);
    in2 = rgeos_Polygons2geospolygon(env, obj2);

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
    
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, ngrps)) == NULL) {
            error("rgeos_SpatialPolygonsUnion: collection not created");
    }

    PROTECT(ans = rgeos_geospolygon2SpatialPolygons(env, GC, p4s, grpIDs, ngrps, thresh)); pc++;

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
        GC = rgeos_Polygons2geospolygon(env, pl);
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





