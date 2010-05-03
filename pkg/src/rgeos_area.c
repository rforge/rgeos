#include "rgeos.h"

void rgeos_csArea(SEXP env, GEOSCoordSeq s, double *area) {
    double val;
    int i, n, pc=0;
    double xc, yc;
    SEXP coords, nn, dims;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        error("rgeos_lrArea: size failure");
        
    PROTECT(coords = NEW_NUMERIC(2*n)); pc++;
    PROTECT(nn = NEW_INTEGER(1)); pc++;
    
    INTEGER_POINTER(nn)[0] = n;
    
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = (int) n;
    INTEGER_POINTER(dims)[1] = (int) 2;

    for (i=0; i<n; i++) {
        if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            error("rgeos_lrArea: x failure");
        }    
        NUMERIC_POINTER(coords)[i] = val;
        if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            error("rgeos_lrArea: y failure");
        }
        NUMERIC_POINTER(coords)[i+n] = val;
    }
    setAttrib(coords, R_DimSymbol, dims);

    SP_PREFIX(spRFindCG_c)(nn, coords, &xc, &yc, area);

    UNPROTECT(pc);

    return;
}
