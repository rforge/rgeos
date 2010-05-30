#ifndef RGEOS_H
#define RGEOS_H

#include <R.h>
#include <Rdefines.h>

#include <geos_c.h>

/* use same define in package's local_stubs.c file */
#define SP_XPORT(x) RGEOS_ ## x
#include "sp.h"

#define R_OFFSET 1

// Utility functions
SEXP rgeos_GEOSversion(void);
SEXP rgeos_Init(void);
SEXP rgeos_finish(SEXP env);
static void rgeos_finish_handle(SEXP ptr);
GEOSContextHandle_t getContextHandle(SEXP env);


double getScale(SEXP env);
double makePrecise(double val, double scale);
double sym_round(double val);
double java_math_round(double val);
double rint_vc(double val);
// Based on geos rounding methods, use just one global 
// round function, so it can be easily switched globally 
double rgeos_round(double val);
// inline removed, see Writing R extensions, section 6.14
// http://cran.r-project.org/doc/manuals/R-exts.html#Inlining-C-functions

SEXP rgeos_double_translate(SEXP env, SEXP obj, SEXP id, SEXP thres);
void printCoordSeq(SEXP env, GEOSCoordSeq s);


// Bounding Box functions - rgeos_bbox.c
SEXP rgeos_geom2bbox(SEXP env, GEOSGeom geom);


// Coordinate sequence and matrix functions - rgeos_coord.c
GEOSCoordSeq rgeos_crdMat2CoordSeq(SEXP env, SEXP mat, SEXP dim);
GEOSCoordSeq rgeos_xy2CoordSeq(SEXP env, double x, double y);

GEOSGeom rgeos_xy2Pt(SEXP env, double x, double y);
GEOSGeom rgeos_crdMat2LineString(SEXP env, SEXP mat, SEXP dim);
GEOSGeom rgeos_crdMat2LinearRing(SEXP env, SEXP mat, SEXP dim);
GEOSGeom rgeos_crdMat2Polygon(SEXP env, SEXP mat, SEXP dim);

SEXP rgeos_CoordSeq2crdMat(SEXP env, GEOSCoordSeq s, int HasZ, int rev);
SEXP rgeos_geospoint2crdMat(SEXP env, GEOSGeom geom, SEXP idlist, int ntotal, int type);


// Translate functions GEOS to R - rgeos_geos2R.c
SEXP rgeos_convert_geos2R(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, SEXP thres);
SEXP rgeos_geospoint2SpatialPoints(SEXP env, GEOSGeom mpt, SEXP p4s, SEXP id, int n);
SEXP rgeos_geosline2SpatialLines(SEXP env, GEOSGeom geom, SEXP p4s, SEXP id, int ng);
SEXP rgeos_geospolygon2SpatialPolygons(SEXP env, GEOSGeom geom, SEXP p4s, SEXP IDs, int ng, SEXP thresh);


SEXP rgeos_GCPolygons(SEXP env, GEOSGeom Geom, char *buf, SEXP thresh);
SEXP rgeos_LinearRingPolygon(SEXP env, GEOSGeom lr, int hole);


//Translate functions R to GEOS - rgeos_R2geos.c
GEOSGeom rgeos_convert_R2geos(SEXP env, SEXP obj);

GEOSGeom rgeos_SpatialPoints2geospoint(SEXP env, SEXP obj);
GEOSGeom rgeos_SpatialLines2geosline(SEXP env, SEXP obj);
GEOSGeom rgeos_Lines2geosline(SEXP env, SEXP obj);

GEOSGeom rgeos_SpatialPolygons2geospolygon(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons2geospolygon(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons2MP(SEXP env, SEXP obj);
GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec);


// WKT Functions - rgeos_wkt.c
SEXP rgeos_readWKT(SEXP env, SEXP obj, SEXP p4s, SEXP id, SEXP thres);
SEXP rgeos_writeWKT(SEXP env, SEXP obj, SEXP byid);


// Topology Functions - rgeos_topology.c
SEXP rgeos_envelope(SEXP env, SEXP obj, SEXP id, SEXP thres);
SEXP rgeos_convexhull(SEXP env, SEXP obj, SEXP id, SEXP thres);
SEXP rgeos_boundary(SEXP env, SEXP obj, SEXP id, SEXP thres); 
SEXP rgeos_getcentroid(SEXP env, SEXP obj, SEXP id, SEXP thres);
SEXP rgeos_pointonsurface(SEXP env, SEXP obj, SEXP id, SEXP thres);

// Buffer Functions - rgeos_buffer.c
SEXP rgeos_buffer(SEXP env, SEXP obj, SEXP byid, SEXP id, SEXP thres, SEXP width, SEXP quadsegs, 
                  SEXP capStyle, SEXP joinStyle, SEXP mitreLimit);


// GPC functions
GEOSGeom GPCptPolygon(SEXP env, SEXP obj);
GEOSGeom GPCpt2LinearRing(SEXP env, SEXP obj);
GEOSCoordSeq GPCpt2CoordSeq(SEXP env, SEXP obj);
GEOSGeom GCPPtsGC(SEXP env, SEXP pls);
GEOSGeom GPCpt_i_Polygon(SEXP env, SEXP pls, SEXP vec);

SEXP SymDiffGpcGEOS(SEXP env, SEXP A, SEXP B);
SEXP UnaryUnionGpcGEOS(SEXP env, SEXP A);
SEXP UnionGpcGEOS(SEXP env, SEXP A, SEXP B);
SEXP IntersectGpcGEOS(SEXP env, SEXP A, SEXP B);
SEXP DiffGpcGEOS(SEXP env, SEXP A, SEXP B);
SEXP checkHolesGPC(SEXP env, SEXP A);

/* SEXP GCpolysGPCpts(SEXP env, GEOSGeom GC); */
SEXP GCGCPPts(SEXP env, GEOSGeom Geom);
SEXP rgeos_LinearRingGCPPts(SEXP env, GEOSGeom lr, int hole);


// Needs to be classified

/*GEOSGeom rgeos_plspairUnion(SEXP env, SEXP ipls, SEXP igrp);*/

void rgeos_csArea(SEXP env, GEOSCoordSeq s, double *area);

GEOSGeom rgeos_plsbufUnion(SEXP env, SEXP ipls, SEXP igrp);

SEXP rgeos_SpatialPolygonsSimplify(SEXP env, SEXP obj, SEXP tolerance, SEXP thresh);

SEXP rgeos_PolygonsContain(SEXP env, SEXP obj);

SEXP rgeos_SpatialPolygonsUnion(SEXP env, SEXP obj, SEXP grps, SEXP grpIDs, SEXP thresh);


SEXP rgeos_Polygons_intersection(SEXP env, SEXP obj1, SEXP obj2);

SEXP rgeos_Lines_intersection(SEXP env, SEXP obj1, SEXP obj2);

SEXP GC_Contains(SEXP env, GEOSGeom GC);

SEXP rgeos_poly_findInBox(SEXP env, SEXP pls, SEXP as_points);


#endif

