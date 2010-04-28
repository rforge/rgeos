#include "rgeos.h"

SEXP rgeos_GEOSversion(void) {

    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GEOSversion()));

    UNPROTECT(1);

    return(ans);
}

static void __errorHandler(const char *fmt, ...) {

    char buf[BUFSIZE], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    error(buf);

    return;
  
}

static void __warningHandler(const char *fmt, ...) {

    char buf[BUFSIZE], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    warning(buf);
    
    return;
}

GEOSContextHandle_t getContextHandle(SEXP env) {

    GEOSContextHandle_t r;
    SEXP ptr;

    ptr = findVarInFrame(env, install("GEOSptr"));
    r = R_ExternalPtrAddr(ptr);

    return(r);

}

SEXP rgeos_Init(void) {

    GEOSContextHandle_t r;
    SEXP sxpHandle;

    r = initGEOS_r((GEOSMessageHandler) __warningHandler, 
        (GEOSMessageHandler) __errorHandler);

    sxpHandle = R_MakeExternalPtr((void *) r, mkChar("GEOSContextHandle"),
        R_NilValue);
    R_RegisterCFinalizerEx(sxpHandle, rgeos_finish_handle, TRUE);
 
    return(sxpHandle);

}

static void rgeos_finish_handle(SEXP ptr) {

    if(!R_ExternalPtrAddr(ptr)) return;
    R_ClearExternalPtr(ptr);
}


SEXP rgeos_finish(SEXP env) {

    GEOSContextHandle_t r;
    SEXP sxpHandle;

    r = getContextHandle(env);

    finishGEOS_r(r);

    sxpHandle = findVarInFrame(env, install("GEOSptr"));

    rgeos_finish_handle(sxpHandle);

    return(R_NilValue);

}

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim) {

    unsigned int i, n, m;
    n = (unsigned int) INTEGER_POINTER(dim)[0];
    m = (unsigned int) INTEGER_POINTER(dim)[1];
    double val;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (m != 2) error("Only 2D geometries permitted");

    GEOSCoordSeq s;

    s = GEOSCoordSeq_create_r(GEOShandle, n, m);

    for(i=0; i<n; i++) {
        val = NUMERIC_POINTER(mat)[i];
        if (GEOSCoordSeq_setX_r(GEOShandle, s, i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: X not set for %d", i);
        }
        val = NUMERIC_POINTER(mat)[i+n];
        if (GEOSCoordSeq_setY_r(GEOShandle, s, i, val) == 0) {
            GEOSCoordSeq_destroy_r(GEOShandle, s);
            error("rgeos_crdMat2CoordSeq: Y not set for %d", i);
        }
    }

    return(s);

}

GEOSCoordSeq rgeos_xy2CoordSeq(SEXP env, double x, double y) {


    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = GEOSCoordSeq_create_r(GEOShandle, (unsigned int) 1, (unsigned int) 2);

    if (GEOSCoordSeq_setX_r(GEOShandle, s, 0, x) == 0) {
        GEOSCoordSeq_destroy_r(GEOShandle, s);
        error("rgeos_xy2CoordSeq: X not set");
    }
    if (GEOSCoordSeq_setY_r(GEOShandle, s, 0, y) == 0) {
        GEOSCoordSeq_destroy_r(GEOShandle, s);
        error("rgeos_xy2CoordSeq: Y not set");
    }

    return(s);

}

GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_xy2CoordSeq(env, x, y);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createPoint_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_xy2Pt: point not created");
    }
    return(gl);

}

GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_crdMat2CoordSeq(env, mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLineString_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_crdMat2LineString: lineString not created");
    }
    return(gl);
}


GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    s = rgeos_crdMat2CoordSeq(env, mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLinearRing_r(GEOShandle, s)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, gl);
        error("rgeos_crdMat2LinearRing: linearRing not created");
    }
    if ((int) GEOSisValid_r(GEOShandle, gl) == 1) {
        if (GEOSNormalize_r(GEOShandle, gl) == -1)
            warning("rgeos_crdMat2LinearRing: normalization failure");
    } else {
        warning("rgeos_crdMat2LinearRing: validity failure");
    }

    return(gl);
}

GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim) {

    GEOSGeom g1, p1;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    g1 = rgeos_crdMat2LinearRing(env, mat, dim);

    if ((p1 = GEOSGeom_createPolygon_r(GEOShandle, g1, NULL,
        (unsigned int) 0)) == NULL) {
        GEOSGeom_destroy_r(GEOShandle, g1);
        error("rgeos_crdMat2Polygon: Polygon not created");
    }

    return(p1);

}

GEOSGeom rgeos_Geom2Env(SEXP env, GEOSGeom Geom) {

    GEOSGeom Env;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if ((Env = GEOSEnvelope_r(GEOShandle, Geom)) == NULL) {
        error("rgeos_Geom2Env: failure extracting envelope");
    }
    return(Env);
}


SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev) {

    int pc=0, i, n, m, ii;
    double val;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    if (GEOSCoordSeq_getSize_r(GEOShandle, s, &n) == 0)
        return(R_NilValue);
    if (GEOSCoordSeq_getDimensions_r(GEOShandle, s, &m) == 0)
        return(R_NilValue);
    if (m == 3 && HasZ == 1)
        warning("rgeos_CoordSeq2crdMat: only 2D coordinates respected");
    
    SEXP ans, dims;
    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = n;
    INTEGER_POINTER(dims)[1] = 2;

    for (i=0; i<n; i++){
        ii = (rev) ? (n-1)-i : i;
        if (GEOSCoordSeq_getX_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }    

        NUMERIC_POINTER(ans)[ii] = val;

        if (GEOSCoordSeq_getY_r(GEOShandle, s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }

        NUMERIC_POINTER(ans)[ii+n] = val;
    }

    setAttrib(ans, R_DimSymbol, dims);
    UNPROTECT(pc);
    return(ans);

}

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


