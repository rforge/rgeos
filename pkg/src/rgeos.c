#include "rgeos.h"

SEXP rgeos_GEOSversion(void) {

    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GEOSversion()));

    UNPROTECT(1);

    return(ans);
}

#define BUFSIZE 8192

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


SEXP rgeos_Init(void) {

    initGEOS((GEOSMessageHandler) __warningHandler, 
        (GEOSMessageHandler) __errorHandler);
 
    return(R_NilValue);

}


SEXP rgeos_finish(void) {

    finishGEOS();

    return(R_NilValue);

}

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP mat, SEXP dim) {

    unsigned int i, n, m;
    n = (unsigned int) INTEGER_POINTER(dim)[0];
    m = (unsigned int) INTEGER_POINTER(dim)[1];

    if (m != 2) error("Only 2D geometries permitted");

    GEOSCoordSeq s;

    s = GEOSCoordSeq_create(n, m);

    for(i=0; i<n; i++) {
        if (GEOSCoordSeq_setX(s, i, NUMERIC_POINTER(mat)[i]) == 0) {
            GEOSCoordSeq_destroy(s);
            error("rgeos_crdMat2CoordSeq: X not set for %d", i);
        }
        if (GEOSCoordSeq_setY(s, i, NUMERIC_POINTER(mat)[i+n]) == 0) {
            GEOSCoordSeq_destroy(s);
            error("rgeos_crdMat2CoordSeq: Y not set for %d", i);
        }
    }

    return(s);

}

GEOSCoordSeq rgeos_xy2CoordSeq(double x, double y) {


    GEOSCoordSeq s;

    s = GEOSCoordSeq_create((unsigned int) 1, (unsigned int) 2);

    if (GEOSCoordSeq_setX(s, 0, x) == 0) {
        GEOSCoordSeq_destroy(s);
        error("rgeos_xy2CoordSeq: X not set");
    }
    if (GEOSCoordSeq_setY(s, 0, y) == 0) {
        GEOSCoordSeq_destroy(s);
        error("rgeos_xy2CoordSeq: Y not set");
    }

    return(s);

}

GEOSGeom rgeos_xy2Pt(double x, double y) {

    GEOSCoordSeq s;

    s = rgeos_xy2CoordSeq(x, y);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createPoint(s)) == NULL) {
        GEOSGeom_destroy(gl);
        error("rgeos_xy2Pt: point not created");
    }
    return(gl);

}

GEOSGeom rgeos_crdMat2LineString(SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    s = rgeos_crdMat2CoordSeq(mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLineString(s)) == NULL) {
        GEOSGeom_destroy(gl);
        error("rgeos_crdMat2LineString: lineString not created");
    }
    return(gl);
}


GEOSGeom rgeos_crdMat2LinearRing(SEXP mat, SEXP dim) {

    GEOSCoordSeq s;

    s = rgeos_crdMat2CoordSeq(mat, dim);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLinearRing(s)) == NULL) {
        GEOSGeom_destroy(gl);
        error("rgeos_crdMat2LinearRing: linearRing not created");
    }
    return(gl);
}

GEOSGeom rgeos_crdMat2Polygon(SEXP mat, SEXP dim) {

    GEOSGeom g1, p1;

    g1 = rgeos_crdMat2LinearRing(mat, dim);

    if ((p1 = GEOSGeom_createPolygon(g1, NULL, (unsigned int) 0)) == NULL) {
        GEOSGeom_destroy(g1);
        error("rgeos_crdMat2Polygon: Polygon not created");
    }

    return(p1);

}

GEOSGeom rgeos_Geom2Env(GEOSGeom Geom) {
    GEOSGeom Env;
    if ((Env = GEOSEnvelope(Geom)) == NULL) {
        error("rgeos_Geom2Env: failure extracting envelope");
    }
    return(Env);
}


SEXP rgeos_CoordSeq2crdMat(GEOSCoordSeq s, int HasZ) {

    int pc=0, i, n, m;
    double val;

    if (GEOSCoordSeq_getSize(s, &n) == 0)
        return(R_NilValue);
    if (GEOSCoordSeq_getDimensions(s, &m) == 0)
        return(R_NilValue);
    if (m == 3 && HasZ == 1)
        warning("rgeos_CoordSeq2crdMat: only 2D coordinates respected");
    
    SEXP ans, dims;
    PROTECT(ans = NEW_NUMERIC(n*2)); pc++;
    PROTECT(dims = NEW_INTEGER(2)); pc++;
    INTEGER_POINTER(dims)[0] = n;
    INTEGER_POINTER(dims)[1] = 2;

    for (i=0; i<n; i++){
        if (GEOSCoordSeq_getX(s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }    

        NUMERIC_POINTER(ans)[i] = val;

        if (GEOSCoordSeq_getY(s, (unsigned int) i, &val) == 0) {
            return(R_NilValue);
        }

        NUMERIC_POINTER(ans)[i+n] = val;
    }

    setAttrib(ans, R_DimSymbol, dims);
    UNPROTECT(pc);
    return(ans);

}


