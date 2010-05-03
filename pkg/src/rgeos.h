#ifndef RGEOS_H
#define RGEOS_H

#include <R.h>
#include <Rdefines.h>

#include <geos_c.h>

/* use same define in package's local_stubs.c file */
#define SP_XPORT(x) RGEOS_ ## x
#include "sp.h"

#define R_OFFSET 1

GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim);

GEOSCoordSeq rgeos_xy2CoordSeq(SEXP env, double x, double y);

GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y);

GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim);

GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim);

GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim);

GEOSGeom rgeos_SPoints2MP(SEXP env, SEXP obj);

GEOSGeom rgeos_SpatialPolygonsGC(SEXP env, SEXP obj);

GEOSGeom rgeos_Polygons2GC(SEXP env, SEXP obj);

GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec);

GEOSGeom rgeos_Lines2GC(SEXP env, SEXP obj);

void rgeos_csArea(SEXP env, GEOSCoordSeq s, double *area);

GEOSGeom rgeos_plsbufUnion(SEXP env, SEXP ipls, SEXP igrp);

/*GEOSGeom rgeos_plspairUnion(SEXP env, SEXP ipls, SEXP igrp);*/

GEOSGeom GPCptPolygon(SEXP env, SEXP obj);

GEOSGeom GPCpt2LinearRing(SEXP env, SEXP obj);

GEOSCoordSeq GPCpt2CoordSeq(SEXP env, SEXP obj);

GEOSGeom GCPPtsGC(SEXP env, SEXP pls);

GEOSGeom GPCpt_i_Polygon(SEXP env, SEXP pls, SEXP vec);

SEXP rgeos_GCSpatialPolygons(SEXP env, GEOSGeom Geom, SEXP p4s, SEXP IDs,
    SEXP thresh);

SEXP rgeos_SpatialPolygonsSimplify(SEXP env, SEXP obj, SEXP tolerance,
    SEXP thresh);

SEXP rgeos_PolygonsContain(SEXP env, SEXP obj);

SEXP rgeos_lineLength(SEXP env, SEXP mat, SEXP dim);

SEXP rgeos_PolArea(SEXP env, SEXP mat, SEXP dim);

SEXP rgeos_PolCentroid(SEXP env, SEXP mat, SEXP dim);

SEXP rgeos_Contains1Pol1pt(SEXP env, SEXP mat, SEXP dim, SEXP x, SEXP y);

SEXP rgeos_Within1Pol1pt(SEXP env, SEXP mat, SEXP dim, SEXP x, SEXP y);

SEXP rgeos_DistNpts1pt(SEXP env, SEXP mat, SEXP dim, SEXP x2, SEXP y2);

SEXP rgeos_Dist1LR1pt(SEXP env, SEXP mat, SEXP dim, SEXP x, SEXP y);

SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev);

SEXP rgeos_MP2crdMat(SEXP env, GEOSGeom GC);

SEXP rgeos_Geom2bbox(SEXP env, GEOSGeom Geom);

SEXP rgeos_GCPolygons(SEXP env, GEOSGeom Geom, char *buf, SEXP thresh);

SEXP rgeos_LinearRingPolygon(SEXP env, GEOSGeom lr, int hole);

SEXP rgeos_SpatialPolygonsUnion(SEXP env, SEXP obj, SEXP grps, SEXP grpIDs, SEXP thresh);

SEXP rgeos_Init(void);

SEXP rgeos_GEOSversion(void);

SEXP rgeos_finish(SEXP env);

static void rgeos_finish_handle(SEXP ptr);

double getScale(SEXP env);

SEXP rgeos_Polygons_intersection(SEXP env, SEXP obj1, SEXP obj2);

SEXP rgeos_Lines_intersection(SEXP env, SEXP obj1, SEXP obj2);

SEXP SymDiffGpcGEOS(SEXP env, SEXP A, SEXP B);

SEXP UnaryUnionGpcGEOS(SEXP env, SEXP A);

SEXP UnionGpcGEOS(SEXP env, SEXP A, SEXP B);

SEXP IntersectGpcGEOS(SEXP env, SEXP A, SEXP B);

SEXP DiffGpcGEOS(SEXP env, SEXP A, SEXP B);

SEXP checkHolesGPC(SEXP env, SEXP A);

SEXP GC_Contains(SEXP env, GEOSGeom GC);

/* SEXP GCpolysGPCpts(SEXP env, GEOSGeom GC); */

SEXP GCGCPPts(SEXP env, GEOSGeom Geom);

SEXP rgeos_LinearRingGCPPts(SEXP env, GEOSGeom lr, int hole);

SEXP rgeos_wkt2sp(SEXP env,SEXP obj,SEXP id,SEXP thres);

GEOSContextHandle_t getContextHandle(SEXP env);

double makePrecise(double val, double scale);
double sym_round(double val);
double java_math_round(double val);
double rint_vc(double val);
// Based on geos rounding methods, use just one global 
// round function, so it can be easily switched globally 
double rgeos_round(double val);
// inline removed, see Writing R extensions, section 6.14
// http://cran.r-project.org/doc/manuals/R-exts.html#Inlining-C-functions

#endif

