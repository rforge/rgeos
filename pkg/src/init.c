#include <R.h>
#include <Rinternals.h>
#include "rgeos.h"

#include <R_ext/Rdynload.h>

static const R_CMethodDef CEntries[] = {
    {"rgeos_crdMat2CoordSeq", (DL_FUNC) &rgeos_crdMat2CoordSeq, 2},
    {"rgeos_xy2CoordSeq", (DL_FUNC) &rgeos_xy2CoordSeq, 2},
    {"rgeos_Geom2Env", (DL_FUNC) &rgeos_Geom2Env, 1},
    {"rgeos_xy2Pt", (DL_FUNC) &rgeos_xy2Pt, 2},
    {"rgeos_crdMat2LineString", (DL_FUNC) &rgeos_crdMat2LineString, 2},
    {"rgeos_crdMat2LinearRing", (DL_FUNC) &rgeos_crdMat2LinearRing, 2},
    {"rgeos_crdMat2Polygon", (DL_FUNC) &rgeos_crdMat2Polygon, 2},
    {"rgeos_SPoints2MP", (DL_FUNC) &rgeos_SPoints2MP, 1},
    {"rgeos_SpatialPolygonsGC", (DL_FUNC) &rgeos_SpatialPolygonsGC, 1},
    {"rgeos_Polygons2GC", (DL_FUNC) &rgeos_Polygons2GC, 1},
    {"rgeos_Polygons_i_2Polygon", (DL_FUNC) &rgeos_Polygons_i_2Polygon, 2},
    {"rgeos_csArea", (DL_FUNC) &rgeos_csArea, 2},
    {"rgeos_plsUnion", (DL_FUNC) &rgeos_plsUnion, 2},
    {NULL, NULL, 0}
};


static R_CallMethodDef CallEntries[] = {

    {"rgeos_SpatialPolygonsUnion", (DL_FUNC) &rgeos_SpatialPolygonsUnion, 4},
    {"rgeos_GCSpatialPolygons", (DL_FUNC) &rgeos_GCSpatialPolygons, 4},
    {"rgeos_SpatialPolygonsSimplify",
        (DL_FUNC) &rgeos_SpatialPolygonsSimplify, 3},
    {"rgeos_PolygonsContain", (DL_FUNC) &rgeos_PolygonsContain, 1},
    {"rgeos_lineLength", (DL_FUNC) &rgeos_lineLength, 2},
    {"rgeos_PolArea", (DL_FUNC) &rgeos_PolArea, 2},
    {"rgeos_PolCentroid", (DL_FUNC) &rgeos_PolCentroid, 2},
    {"rgeos_Contains1Pol1pt", (DL_FUNC) &rgeos_Contains1Pol1pt, 4},
    {"rgeos_Within1Pol1pt", (DL_FUNC) &rgeos_Within1Pol1pt, 4},
    {"rgeos_DistNpts1pt", (DL_FUNC) &rgeos_DistNpts1pt, 4},
    {"rgeos_Dist1LR1pt", (DL_FUNC) &rgeos_Dist1LR1pt, 4},
    {"rgeos_CoordSeq2crdMat", (DL_FUNC) &rgeos_CoordSeq2crdMat, 3},
    {"rgeos_MP2crdMat", (DL_FUNC) &rgeos_MP2crdMat, 1},
    {"rgeos_Geom2bbox", (DL_FUNC) &rgeos_Geom2bbox, 1},
    {"rgeos_GCPolygons", (DL_FUNC) &rgeos_GCPolygons, 3},
    {"rgeos_LinearRingPolygon", (DL_FUNC) &rgeos_LinearRingPolygon, 2},
    {"rgeos_Init", (DL_FUNC) &rgeos_Init, 0},
    {"finishGEOS", (DL_FUNC) &finishGEOS, 0},
    {"rgeos_GEOSversion", (DL_FUNC) &rgeos_GEOSversion, 0},
    {"rgeos_Polygons_intersection", (DL_FUNC) &rgeos_Polygons_intersection, 2},
    {NULL, NULL, 0}
};

void 
#ifdef HAVE_VISIBILITY_ATTRIBUTE
__attribute__ ((visibility ("default")))
#endif
R_init_rgeos(DllInfo *dll) {

    SEXP INIT;

    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    INIT = rgeos_Init();

}
