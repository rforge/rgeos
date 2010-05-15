#include <R.h>
#include <Rinternals.h>
#include "rgeos.h"

#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[] = {
    {"rgeos_crdMat2CoordSeq", (DL_FUNC) &rgeos_crdMat2CoordSeq, 3},
    {"rgeos_xy2CoordSeq", (DL_FUNC) &rgeos_xy2CoordSeq, 3},
    {"rgeos_xy2Pt", (DL_FUNC) &rgeos_xy2Pt, 3},
    {"rgeos_crdMat2LineString", (DL_FUNC) &rgeos_crdMat2LineString, 3},
    {"rgeos_crdMat2LinearRing", (DL_FUNC) &rgeos_crdMat2LinearRing, 3},
    {"rgeos_crdMat2Polygon", (DL_FUNC) &rgeos_crdMat2Polygon, 3},
    {"rgeos_SPoints2MP", (DL_FUNC) &rgeos_SPoints2MP, 2},
    {"rgeos_SpatialPolygonsGC", (DL_FUNC) &rgeos_SpatialPolygonsGC, 2},
    {"rgeos_Polygons2GC", (DL_FUNC) &rgeos_Polygons2GC, 2},
    {"rgeos_Liness2GC", (DL_FUNC) &rgeos_Lines2GC, 2},
    {"rgeos_Polygons_i_2Polygon", (DL_FUNC) &rgeos_Polygons_i_2Polygon, 3},
    {"rgeos_csArea", (DL_FUNC) &rgeos_csArea, 3},
    {"rgeos_plsbufUnion", (DL_FUNC) &rgeos_plsbufUnion, 3},
    {NULL, NULL, 0}
};


static R_CallMethodDef CallEntries[] = {

    {"rgeos_SpatialPolygonsUnion", (DL_FUNC) &rgeos_SpatialPolygonsUnion, 5},
    {"rgeos_GCSpatialPolygons", (DL_FUNC) &rgeos_GCSpatialPolygons, 5},
    {"rgeos_SpatialPolygonsSimplify", (DL_FUNC) &rgeos_SpatialPolygonsSimplify, 4},
    {"rgeos_PolygonsContain", (DL_FUNC) &rgeos_PolygonsContain, 2},
    {"rgeos_lineLength", (DL_FUNC) &rgeos_lineLength, 3},
    {"rgeos_PolArea", (DL_FUNC) &rgeos_PolArea, 3},
    {"rgeos_PolCentroid", (DL_FUNC) &rgeos_PolCentroid, 3},
    {"rgeos_Contains1Pol1pt", (DL_FUNC) &rgeos_Contains1Pol1pt, 5},
    {"rgeos_Within1Pol1pt", (DL_FUNC) &rgeos_Within1Pol1pt, 5},
    {"rgeos_DistNpts1pt", (DL_FUNC) &rgeos_DistNpts1pt, 5},
    {"rgeos_Dist1LR1pt", (DL_FUNC) &rgeos_Dist1LR1pt, 5},
    {"rgeos_CoordSeq2crdMat", (DL_FUNC) &rgeos_CoordSeq2crdMat, 4},
    {"rgeos_multipoint2crdMat", (DL_FUNC) &rgeos_multipoint2crdMat, 2},
    {"rgeos_Geom2bbox", (DL_FUNC) &rgeos_Geom2bbox, 2},
    {"rgeos_GCPolygons", (DL_FUNC) &rgeos_GCPolygons, 4},
    {"rgeos_LinearRingPolygon", (DL_FUNC) &rgeos_LinearRingPolygon, 3},
    {"rgeos_Init", (DL_FUNC) &rgeos_Init, 0},
    {"rgeos_finish", (DL_FUNC) &rgeos_finish, 1},
    {"rgeos_GEOSversion", (DL_FUNC) &rgeos_GEOSversion, 0},
    {"rgeos_Polygons_intersection", (DL_FUNC) &rgeos_Polygons_intersection, 3},
    {"rgeos_Lines_intersection", (DL_FUNC) &rgeos_Lines_intersection, 3},
    {"SymDiffGpcGEOS", (DL_FUNC) &SymDiffGpcGEOS, 3},
    {"DiffGpcGEOS", (DL_FUNC) &DiffGpcGEOS, 3},
    {"UnionGpcGEOS", (DL_FUNC) &UnionGpcGEOS, 3},
    {"IntersectGpcGEOS", (DL_FUNC) &IntersectGpcGEOS, 3},
    {"UnaryUnionGpcGEOS", (DL_FUNC) &UnaryUnionGpcGEOS, 2},
    {"checkHolesGPC", (DL_FUNC) &checkHolesGPC, 2},
<<<<<<< .mine
    {"rgeos_wkt2sp", (DL_FUNC) &rgeos_wkt2sp,4},
    {"rgeos_readWKT", (DL_FUNC) &rgeos_readWKT,4}, 
=======
    {"rgeos_wkt2sp", (DL_FUNC) &rgeos_wkt2sp, 4}, 
    {"rgeos_poly_findInBox", (DL_FUNC) &rgeos_poly_findInBox, 3}, 
>>>>>>> .r63
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
