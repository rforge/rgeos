#include "rgeos.h"

SEXP rgeos_wkt2sp(SEXP env,SEXP obj,SEXP id,SEXP thres) {
    GEOSGeom GC;
    SEXP ans;
    int pc=0;
    
    GEOSWKTReader *reader;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    PROTECT(obj = AS_CHARACTER(obj)); pc++;
    
    reader=GEOSWKTReader_create_r(GEOShandle);
    
    if( (GC = GEOSWKTReader_read_r(GEOShandle,reader, CHAR(STRING_ELT(obj, 0)) ) )==NULL) 
        error("rgeos_wkt2sp: unable to parse wkt");
    
    GEOSWKTReader_destroy_r(GEOShandle,reader);
    
    
    PROTECT( ans = rgeos_GCPolygons(env, GC, CHAR(STRING_ELT(id, 0)), thres)); pc++;
    UNPROTECT(pc);
    
    return(ans);
}