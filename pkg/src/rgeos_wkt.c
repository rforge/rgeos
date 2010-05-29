#include "rgeos.h"


SEXP rgeos_readWKT(SEXP env, SEXP obj, SEXP p4s, SEXP id, SEXP thres) {
    
    GEOSGeom geom;
    SEXP ans;
    int type, pc=0;
    char ibuf[BUFSIZ];
    
    GEOSWKTReader *reader;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    
    reader = GEOSWKTReader_create_r(GEOShandle);
    
    geom = GEOSWKTReader_read_r(GEOShandle,reader, CHAR(STRING_ELT(obj, 0)));
    GEOSWKTReader_destroy_r(GEOShandle,reader);
    
    if (geom == NULL) error("rgeos_readWKT: unable to read wkt");
    
    ans = rgeos_convert_geos2R(env, geom, p4s, id, thres);
    
    //TODO - Why can't geom be destroyed here?
    //if (geom != NULL) GEOSGeom_destroy_r(GEOShandle, geom);
    
    return(ans);
}


SEXP rgeos_writeWKT(SEXP env, SEXP obj, SEXP byid) {
    
    GEOSGeom geom, curgeom;
    SEXP ans;
    int type, n, i, pc=0;
    char *buf;
    
    GEOSWKTWriter *writer;
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    writer = GEOSWKTWriter_create_r(GEOShandle);
    geom = rgeos_convert_R2geos(env, obj);
    
    
    if (LOGICAL_POINTER(byid)[0])
        n = GEOSGetNumGeometries_r(GEOShandle, geom);
    else
        n = 1;
        
    PROTECT(ans = NEW_CHARACTER(n)); pc++;
    
    curgeom = geom;
    for(i=0; i<n; i++) {
        if ( n > 1) {
            curgeom = (GEOSGeom) GEOSGetGeometryN_r(GEOShandle, geom, i);
            if (curgeom == NULL) error("rgeos_writeWKT: unable to get subgeometries");
        }
        
        buf = GEOSWKTWriter_write_r(GEOShandle, writer, curgeom);
        if (buf == NULL) error("rgeos_writeWKT: unable to write wkt");
        SET_STRING_ELT(ans, i, COPY_TO_USER_STRING(buf));
        
        GEOSFree_r(GEOShandle, buf);
    }
    
    GEOSWKTWriter_destroy_r(GEOShandle,writer);
    GEOSGeom_destroy_r(GEOShandle, geom);
    
    UNPROTECT(pc);
    
    return(ans);
}