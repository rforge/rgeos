#include "rgeos.h"

SEXP rgeos_DistNpts1pt(SEXP mat, SEXP dim, SEXP x2, SEXP y2) {

    int pc=0;
    unsigned int i, n;
    n = (unsigned int) INTEGER_POINTER(dim)[0];

    GEOSGeom p1, p2;
    double dist;
    SEXP ans;

    p2 = rgeos_xy2Pt(NUMERIC_POINTER(x2)[0], NUMERIC_POINTER(y2)[0]);
    PROTECT(ans = NEW_NUMERIC(n)); pc++;

    for (i=0; i<n; i++) {
        p1 = rgeos_xy2Pt(NUMERIC_POINTER(mat)[i], NUMERIC_POINTER(mat)[i+n]);
        if (GEOSDistance(p1, p2, &dist) == 0) {
            GEOSGeom_destroy(p1);
            GEOSGeom_destroy(p2);
            error("rgeos_ptsDist: problem measuring distance");
        }
        GEOSGeom_destroy(p1);
        NUMERIC_POINTER(ans)[i] = dist;
    }
    GEOSGeom_destroy(p2);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_Dist1Ln1pt(SEXP mat, SEXP dim, SEXP x, SEXP y) {

    int pc=0;

    GEOSGeom g1, g2;

    g1 = rgeos_crdMat2LineString(mat, dim);

    g2 = rgeos_xy2Pt(NUMERIC_POINTER(x)[0], NUMERIC_POINTER(y)[0]);

    double dist;
    if (GEOSDistance(g1, g2, &dist) == 0) {
        GEOSGeom_destroy(g1);
        GEOSGeom_destroy(g2);
        error("rgeos_Dist1Ln1pt: problem measuring distance");
    }
    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);

    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = dist;
    UNPROTECT(pc);
    return(ans);

}


SEXP rgeos_Dist1LR1pt(SEXP mat, SEXP dim, SEXP x, SEXP y) {

    int pc=0;

    GEOSGeom g1, g2;

    g1 = rgeos_crdMat2LinearRing(mat, dim);

    g2 = rgeos_xy2Pt(NUMERIC_POINTER(x)[0], NUMERIC_POINTER(y)[0]);

    double dist;
    if (GEOSDistance(g1, g2, &dist) == 0) {
        GEOSGeom_destroy(g1);
        GEOSGeom_destroy(g2);
        error("rgeos_Dist1Ln1pt: problem measuring distance");
    }
    GEOSGeom_destroy(g1);
    GEOSGeom_destroy(g2);

    SEXP ans;
    PROTECT(ans = NEW_NUMERIC(1)); pc++;
    NUMERIC_POINTER(ans)[0] = dist;
    UNPROTECT(pc);
    return(ans);

}

