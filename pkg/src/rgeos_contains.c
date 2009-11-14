#include "rgeos.h"

SEXP rgeos_Contains1Pol1pt(SEXP mat, SEXP dim, SEXP x, SEXP y) {

    int pc=0;
    GEOSGeom g2, p1;

    p1 = rgeos_crdMat2Polygon(mat, dim);

    g2 = rgeos_xy2Pt(NUMERIC_POINTER(x)[0], NUMERIC_POINTER(y)[0]);

    int contains;
    if ((contains = (int) GEOSContains(p1, g2)) == 2) {
        GEOSGeom_destroy(p1);
        GEOSGeom_destroy(g2);
        error("rgeos_Contains1LR1pt: problem measuring contains");
    }
    GEOSGeom_destroy(p1);
    GEOSGeom_destroy(g2);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = contains;
    UNPROTECT(pc);
    return(ans);

}


SEXP rgeos_Within1Pol1pt(SEXP mat, SEXP dim, SEXP x, SEXP y) {

    int pc=0;
    GEOSGeom g2, p1;

    p1 = rgeos_crdMat2Polygon(mat, dim);

    g2 = rgeos_xy2Pt(NUMERIC_POINTER(x)[0], NUMERIC_POINTER(y)[0]);

    int within;
    if ((within = (int) GEOSWithin(g2, p1)) == 2) {
        GEOSGeom_destroy(p1);
        GEOSGeom_destroy(g2);
        error("rgeos_Within1LR1pt: problem measuring within");
    }
    GEOSGeom_destroy(p1);
    GEOSGeom_destroy(g2);

    SEXP ans;
    PROTECT(ans = NEW_LOGICAL(1)); pc++;
    LOGICAL_POINTER(ans)[0] = within;
    UNPROTECT(pc);
    return(ans);

}


