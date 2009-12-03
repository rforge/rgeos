#include "rgeos.h"

SEXP rgeos_lineLength(SEXP mat, SEXP dim) {

    int pc=0;

    GEOSGeom gl;

    gl = rgeos_crdMat2LineString(mat, dim);

    double len;
    if (GEOSLength(gl, &len) == 0) {
        GEOSGeom_destroy(gl);
        error("rgeos_lineLength: problem measuring length");
    }
    GEOSGeom_destroy(gl);
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = len;
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolArea(SEXP mat, SEXP dim){

    int pc=0;

    GEOSGeom gl;

    gl = rgeos_crdMat2Polygon(mat, dim);

    double area;
    if (GEOSArea(gl, &area) == 0) {
        GEOSGeom_destroy(gl);
        error("rgeos_PolArea: problem measuring area");
    }
    GEOSGeom_destroy(gl);
    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = area;
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolCentroid(SEXP mat, SEXP dim) {

    GEOSGeom gl, ct;

    gl = rgeos_crdMat2Polygon(mat, dim);
    if ((ct = GEOSGetCentroid(gl)) == 0) {
        GEOSGeom_destroy(gl);
        error("rgeos_PolCentroid: problem measuring centroid");
    }

    GEOSCoordSeq s;

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq(ct)) == NULL) {
        GEOSGeom_destroy(gl);
        GEOSGeom_destroy(ct);
        error("rgeos_PolCentroid: problem extracting centroid");
    }

    SEXP ans;
    
    ans = rgeos_CoordSeq2crdMat(s, (int) GEOSHasZ(ct), FALSE);

    GEOSGeom_destroy(gl);
    GEOSGeom_destroy(ct);

    if (ans == R_NilValue) 
        error("rgeos_PolCentroid: problem extracting coordinates");

    return(ans);

}
