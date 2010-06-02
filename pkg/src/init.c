#include <R.h>
#include <Rinternals.h>
#include "rgeos.h"

#include <R_ext/Rdynload.h>

// TODO - do any of these functions need to be called from R?

static const R_CMethodDef CEntries[] = {
    /*
    {"rgeos_crdMat2CoordSeq", (DL_FUNC) &rgeos_crdMat2CoordSeq, 3},
    {"rgeos_xy2CoordSeq", (DL_FUNC) &rgeos_xy2CoordSeq, 3},
    {"rgeos_xy2Pt", (DL_FUNC) &rgeos_xy2Pt, 3},
    {"rgeos_crdMat2LineString", (DL_FUNC) &rgeos_crdMat2LineString, 3},
    {"rgeos_crdMat2LinearRing", (DL_FUNC) &rgeos_crdMat2LinearRing, 3},
    {"rgeos_crdMat2Polygon", (DL_FUNC) &rgeos_crdMat2Polygon, 3},
    {"rgeos_SPoints2MP", (DL_FUNC) &rgeos_SPoints2MP, 2},
    {"rgeos_SpatialPolygons2geospolygon", (DL_FUNC) &rgeos_SpatialPolygons2geospolygon, 2},
    {"rgeos_Polygons2geospolygon", (DL_FUNC) &rgeos_Polygons2geospolygon, 2},
    {"rgeos_Lines2geosline", (DL_FUNC) &rgeos_Lines2geosline, 2},
    {"rgeos_Polygons_i_2Polygon", (DL_FUNC) &rgeos_Polygons_i_2Polygon, 3},
    {"rgeos_csArea", (DL_FUNC) &rgeos_csArea, 3},
    {"rgeos_plsbufUnion", (DL_FUNC) &rgeos_plsbufUnion, 3},
    {NULL, NULL, 0}
    */
};


static R_CallMethodDef CallEntries[] = {
    
    //Utility Functions
    {"rgeos_Init", (DL_FUNC) &rgeos_Init, 0},
    {"rgeos_finish", (DL_FUNC) &rgeos_finish, 1},
    {"rgeos_GEOSversion", (DL_FUNC) &rgeos_GEOSversion, 0},
    {"rgeos_double_translate", (DL_FUNC) &rgeos_double_translate, 4},
    //WKT Functions
    {"rgeos_readWKT", (DL_FUNC) &rgeos_readWKT,5}, 
    {"rgeos_writeWKT", (DL_FUNC) &rgeos_writeWKT, 3}, 
    
    //Topology Functions
    {"rgeos_envelope", (DL_FUNC) &rgeos_envelope, 5},
    {"rgeos_convexhull", (DL_FUNC) &rgeos_convexhull, 5},
    {"rgeos_boundary", (DL_FUNC) &rgeos_boundary, 5},
    {"rgeos_getcentroid", (DL_FUNC) &rgeos_getcentroid, 5},
    {"rgeos_pointonsurface", (DL_FUNC) &rgeos_pointonsurface, 5},
    {"rgeos_linemerge", (DL_FUNC) &rgeos_linemerge, 5},

    //Binary Predicate Functions
    {"rgeos_relatepattern", (DL_FUNC) &rgeos_relatepattern, 4},
    {"rgeos_disjoint", (DL_FUNC) &rgeos_disjoint, 4},
    {"rgeos_touches", (DL_FUNC) &rgeos_touches, 4},
    {"rgeos_intersects", (DL_FUNC) &rgeos_intersects, 4},
    {"rgeos_crosses", (DL_FUNC) &rgeos_crosses, 4},
    {"rgeos_within", (DL_FUNC) &rgeos_within, 4},
    {"rgeos_contains", (DL_FUNC) &rgeos_contains, 4},
    {"rgeos_overlaps", (DL_FUNC) &rgeos_overlaps, 4},
    {"rgeos_equals", (DL_FUNC) &rgeos_equals, 4},

    //Buffer Functions
    {"rgeos_buffer", (DL_FUNC) &rgeos_buffer, 10},
    
    //Misc functions
    {"rgeos_area", (DL_FUNC) &rgeos_area, 3},
    {"rgeos_length", (DL_FUNC) &rgeos_length, 3},
    {"rgeos_distance", (DL_FUNC) &rgeos_distance, 4},
    {"rgeos_hausdorffdistance", (DL_FUNC) &rgeos_hausdorffdistance, 4},
    
    {"rgeos_SpatialPolygonsUnion", (DL_FUNC) &rgeos_SpatialPolygonsUnion, 5},
    {"rgeos_SpatialPolygonsSimplify", (DL_FUNC) &rgeos_SpatialPolygonsSimplify, 4},
    {"rgeos_PolygonsContain", (DL_FUNC) &rgeos_PolygonsContain, 2},
    {"rgeos_Polygons_intersection", (DL_FUNC) &rgeos_Polygons_intersection, 3},
    {"rgeos_Lines_intersection", (DL_FUNC) &rgeos_Lines_intersection, 3},
    {"SymDiffGpcGEOS", (DL_FUNC) &SymDiffGpcGEOS, 3},
    {"DiffGpcGEOS", (DL_FUNC) &DiffGpcGEOS, 3},
    {"UnionGpcGEOS", (DL_FUNC) &UnionGpcGEOS, 3},
    {"IntersectGpcGEOS", (DL_FUNC) &IntersectGpcGEOS, 3},
    {"UnaryUnionGpcGEOS", (DL_FUNC) &UnaryUnionGpcGEOS, 2},
    {"checkHolesGPC", (DL_FUNC) &checkHolesGPC, 2},
    {"rgeos_poly_findInBox", (DL_FUNC) &rgeos_poly_findInBox, 3}, 
    {NULL, NULL, 0}
};

void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_rgeos(DllInfo *dll) {

//    SEXP INIT;

    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

//    INIT = rgeos_Init();

}
