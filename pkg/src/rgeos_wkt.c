#include "rgeos.h"

SEXP rgeos_wkt2sp(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    GEOSGeom GC;
    SEXP ans;
    int pc=0;
    char ibuf[BUFSIZ];
    
    GEOSWKTReader *reader;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    PROTECT(obj = AS_CHARACTER(obj)); pc++;
    
    reader=GEOSWKTReader_create_r(GEOShandle);
    
    if( (GC = GEOSWKTReader_read_r(GEOShandle,reader, CHAR(STRING_ELT(obj, 0)) ) )==NULL) 
        error("rgeos_wkt2sp: unable to parse wkt");
    
    GEOSWKTReader_destroy_r(GEOShandle,reader);
    
    
    strcpy(ibuf, CHAR(STRING_ELT(id, 0)));
    PROTECT( ans = rgeos_GCPolygons(env, GC, ibuf, thres)); pc++;
    UNPROTECT(pc);
    
    return(ans);
}

SEXP rgeos_readWKT(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    GEOSGeom GC;
    SEXP ans;
    int type, pc=0;
    char ibuf[BUFSIZ];
    
    GEOSWKTReader *reader;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    PROTECT(obj = AS_CHARACTER(obj)); pc++;
    
    reader=GEOSWKTReader_create_r(GEOShandle);
    
    if( (GC = GEOSWKTReader_read_r(GEOShandle,reader, CHAR(STRING_ELT(obj, 0)) ) )==NULL) 
        error("rgeos_readWKT: unable to parse wkt");
    
    GEOSWKTReader_destroy_r(GEOShandle,reader);
    
    
    strcpy(ibuf, CHAR(STRING_ELT(id, 0)));
    
    type = GEOSGeomTypeId_r(GEOShandle, GC);
    switch(type) { // Determine appropriate type for the collection
        case -1:
            error("rgeos_readWKT: unknown geometry type");
            break;
        case GEOS_POINT:
        case GEOS_MULTIPOINT:
            PROTECT( ans = rgeos_multipoint2SpatialPoints(env, GC) ); pc++;
            break;
        
        case GEOS_POLYGON:
            PROTECT( ans = rgeos_GCPolygons(env, GC, ibuf, thres) ); pc++;
            break;
        
        case GEOS_LINESTRING:
        case GEOS_MULTILINESTRING:
        case GEOS_LINEARRING:
        case GEOS_MULTIPOLYGON:
        case GEOS_GEOMETRYCOLLECTION:
            PROTECT( ans = R_NilValue ); pc++;
    }
    
    
    UNPROTECT(pc);
    return(ans);
}
