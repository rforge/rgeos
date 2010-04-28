#include "rgeos.h"

SEXP rgeos_lineLength(SEXP env, SEXP mat, SEXP dim) {

    int pc=0;

    GEOSGeom gl;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    gl = rgeos_crdMat2LineString(env, mat, dim);

    double len;
    if (GEOSLength_r(GEOShandle, gl, &len) == 0) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_lineLength: problem measuring length");
    }
    GEOSGeom_destroy_r(GEOShandle, gl);
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = len;
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolArea(SEXP env, SEXP mat, SEXP dim){

    int pc=0;

    GEOSGeom gl;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    gl = rgeos_crdMat2Polygon(env, mat, dim);

    double area;
    if (GEOSArea_r(GEOShandle, gl, &area) == 0) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_PolArea: problem measuring area");
    }
    GEOSGeom_destroy_r(GEOShandle, gl);
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = area;
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolCentroid(SEXP env, SEXP mat, SEXP dim) {

    GEOSGeom gl, ct;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    gl = rgeos_crdMat2Polygon(env, mat, dim);
    if ((ct = GEOSGetCentroid_r(GEOShandle, gl)) == 0) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_PolCentroid: problem measuring centroid");
    }

    GEOSCoordSeq s;

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, ct))
         == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        GEOSGeom_destroy_r(GEOShandle, ct);
        error("rgeos_PolCentroid: problem extracting centroid");
    }

    SEXP ans;
    
    ans = rgeos_CoordSeq2crdMat(env, s, (int) GEOSHasZ(ct), FALSE);

    GEOSGeom_destroy_r(GEOShandle, gl);
    GEOSGeom_destroy_r(GEOShandle, ct);

    if (ans == R_NilValue) 
        error("rgeos_PolCentroid: problem extracting coordinates");

    return(ans);

}
