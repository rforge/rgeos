#include "rgeos.h"

SEXP GC_Contains(SEXP env, GEOSGeom GC) {

    SEXP ans, dim;
    int pc=0;
    unsigned int i, j, n;
    int contains, ident;

    GEOSGeom Pi, Pj;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ( GEOSisValid_r(GEOShandle, GC) ) {
        GEOSGeom_destroy_r(GEOShandle, GC);
        return(R_NilValue);
    }

    n = (unsigned int) GEOSGetNumGeometries_r(GEOShandle, GC);
    PROTECT(ans = NEW_LIST(2)); pc++;
    PROTECT(SET_VECTOR_ELT(ans, 0, NEW_LOGICAL((int) (n*n)))); pc++;
    PROTECT(SET_VECTOR_ELT(ans, 1, NEW_LOGICAL((int) (n*n)))); pc++;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = (int) n;
    INTEGER_POINTER(dim)[1] = (int) n;
    setAttrib(VECTOR_ELT(ans, 0), R_DimSymbol, dim);
    setAttrib(VECTOR_ELT(ans, 1), R_DimSymbol, dim);

    for (i=0; i<n; i++) {
        if ((Pi = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC, (int) i)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, GC);
            return(R_NilValue);
        } // Pi invalid
            for (j=0; j<n; j++) {
                if ((Pj = (GEOSGeometry *) GEOSGetGeometryN_r(GEOShandle, GC, (int) j)) == NULL) {
                    GEOSGeom_destroy_r(GEOShandle, GC);
                    return(R_NilValue);
                } // Pj invalid
                if (i == j) {
                    LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = FALSE;
                    LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = FALSE;
                } else { // i == j
                    contains = (int) GEOSContains_r(GEOShandle, Pi, Pj);
                    if (contains == 2) {
                        LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = NA_LOGICAL;
                        LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = NA_LOGICAL;
                    } else { // contains invalid
                        ident = (int) GEOSEquals_r(GEOShandle, Pi, Pj);
                        if (ident == 2) {
                            LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = NA_LOGICAL;
                            LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = NA_LOGICAL;
                        } else { // ident invalid
                            LOGICAL_POINTER(VECTOR_ELT(ans, 0))[i+(j*n)] = contains;
                            LOGICAL_POINTER(VECTOR_ELT(ans, 1))[i+(j*n)] = ident;
                        } // ident valid
                    } // contains valid
                } // i != j
            } // j

    } // i
    GEOSGeom_destroy_r(GEOShandle, GC);

    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_PolygonsContain(SEXP env, SEXP obj) {

    GEOSGeom GC;
    GC = rgeos_Polygons2GC(env, obj);

    return(GC_Contains(env, GC));

}