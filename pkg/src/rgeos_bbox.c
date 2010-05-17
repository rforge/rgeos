#include "rgeos.h"

SEXP rgeos_Geom2bbox(SEXP env, GEOSGeom Geom) {

    GEOSGeom bb, bbER;
    GEOSCoordSeq s;
    unsigned int i, n;
    double UX=-DBL_MAX, LX=DBL_MAX, UY=-DBL_MAX, LY=DBL_MAX;
    SEXP bbmat, ans, dim, dimnames;
    int pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ((bb = GEOSEnvelope_r(GEOShandle, Geom)) == NULL) {
        return(R_NilValue);
    }

    if ((bbER = (GEOSGeometry *) GEOSGetExteriorRing_r(GEOShandle, bb)) == NULL) {
        return(R_NilValue);
    }

    if ((s = (GEOSCoordSequence *) GEOSGeom_getCoordSeq_r(GEOShandle, bbER)) == NULL) {
        return(R_NilValue);
    }
    
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    if (n == 0) {
        return(R_NilValue);
    }
    
    bbmat = rgeos_CoordSeq2crdMat(env, s, (int) GEOSHasZ_r(GEOShandle, bb), FALSE); 
    for (i=0; i<n; i++) {
       if (NUMERIC_POINTER(bbmat)[i] > UX) UX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] > UY) UY = NUMERIC_POINTER(bbmat)[i+n];
       if (NUMERIC_POINTER(bbmat)[i] < LX) LX = NUMERIC_POINTER(bbmat)[i];
       if (NUMERIC_POINTER(bbmat)[i+n] < LY) LY = NUMERIC_POINTER(bbmat)[i+n];
    }

    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = LX;
    NUMERIC_POINTER(ans)[1] = LY;
    NUMERIC_POINTER(ans)[2] = UX;
    NUMERIC_POINTER(ans)[3] = UY;
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(ans, R_DimSymbol, dim);
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    
    GEOSCoordSeq_destroy_r(GEOShandle,s);
    
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_initbbox() {
    SEXP ans, dim, dimnames;
    int pc=0;    

    PROTECT(ans = NEW_NUMERIC(4)); pc++;
    NUMERIC_POINTER(ans)[0] = DBL_MAX;
    NUMERIC_POINTER(ans)[1] = DBL_MAX;
    NUMERIC_POINTER(ans)[2] = -DBL_MAX;
    NUMERIC_POINTER(ans)[3] = -DBL_MAX;
     
    UNPROTECT(pc);
    return(ans);
}

SEXP rgeos_formatbbox(SEXP bbox) {
    SEXP dim, dimnames;
    int pc=0;    
 
    PROTECT(dim = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dim)[0] = 2;
    INTEGER_POINTER(dim)[1] = 2;
    setAttrib(bbox, R_DimSymbol, dim);
    
    PROTECT(dimnames = NEW_LIST(2)); pc++;
    SET_VECTOR_ELT(dimnames, 0, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 0, COPY_TO_USER_STRING("x"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 0), 1, COPY_TO_USER_STRING("y"));
    SET_VECTOR_ELT(dimnames, 1, NEW_CHARACTER(2));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 0, COPY_TO_USER_STRING("min"));
    SET_STRING_ELT(VECTOR_ELT(dimnames, 1), 1, COPY_TO_USER_STRING("max"));
    setAttrib(bbox, R_DimNamesSymbol, dimnames);

    UNPROTECT(pc);
    return(bbox);
}

void rgeos_updatebbox_crdmat(SEXP curbbox, SEXP crdmat, unsigned int n ) {
    
    int i;    
    
    for (i=0; i<n; i++) {
        if (NUMERIC_POINTER(crdmat)[i]  > NUMERIC_POINTER(curbbox)[2]) 
            NUMERIC_POINTER(curbbox)[2] = NUMERIC_POINTER(crdmat)[i];
        
        if (NUMERIC_POINTER(crdmat)[i+n]> NUMERIC_POINTER(curbbox)[3]) 
            NUMERIC_POINTER(curbbox)[3] = NUMERIC_POINTER(crdmat)[i+n];
        
        if (NUMERIC_POINTER(crdmat)[i]  < NUMERIC_POINTER(curbbox)[0])
            NUMERIC_POINTER(curbbox)[0] = NUMERIC_POINTER(crdmat)[i];
        
        if (NUMERIC_POINTER(crdmat)[i+n]< NUMERIC_POINTER(curbbox)[1])
            NUMERIC_POINTER(curbbox)[1] = NUMERIC_POINTER(crdmat)[i+n];
    }
    
    
    return;
}