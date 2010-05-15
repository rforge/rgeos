#include "rgeos.h"

// Spatial Points to geometry collection (Multipoints)
GEOSGeom rgeos_SPoints2MP(SEXP env, SEXP obj) {

    int pc=0;
    unsigned int i, n;
    GEOSGeom *geoms;
    GEOSGeom pt, GC;
    SEXP crds, dim;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    crds = GET_SLOT(obj, install("coords")); 
    dim = getAttrib(crds, install("dim")); 
    n = INTEGER_POINTER(dim)[0];

    geoms = (GEOSGeom *) R_alloc((size_t) n, sizeof(GEOSGeom));

    for (i=0; i<n; i++) {
        pt = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[i], NUMERIC_POINTER(crds)[i+n]);
        geoms[i] = pt;
    }

    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, geoms, n)) == NULL) {
        error("rgeos_SPoints2MP: collection not created");
    }

    return(GC);
}

// Lines to geometry collection (Multilinestring)
GEOSGeom rgeos_Lines2GC(SEXP env, SEXP obj) {

    SEXP lns, crdMat, dim;
    GEOSGeom *geoms;
    GEOSGeom ls, GC;
    int nlns, i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(lns = GET_SLOT(obj, install("Lines"))); pc++;
    nlns = length(lns);


    geoms = (GEOSGeom *) R_alloc((size_t) nlns, sizeof(GEOSGeom));

    for (i=0; i<nlns; i++) {
        crdMat = GET_SLOT(VECTOR_ELT(lns, i), install("coords"));
        dim = getAttrib(crdMat, R_DimSymbol);
        ls = rgeos_crdMat2LineString(env, crdMat, dim);
        geoms[i] = ls;
    }
    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTILINESTRING,
        geoms, nlns)) == NULL) {
        error("Lines2GC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);

}

// Spatial polygons to geometry collection (multipolygon)
GEOSGeom rgeos_Polygons2GC(SEXP env, SEXP obj) {

    SEXP pls, crdMat, dim, comm;
    GEOSGeom *geoms;
    GEOSGeom Pol, GC;
    int npls, i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    npls = length(pls);

    PROTECT(comm = SP_PREFIX(comment2comm)(obj)); pc++;

    if (comm == R_NilValue) {

        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (i=0; i<npls; i++) {
            crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(crdMat, R_DimSymbol);
            Pol = rgeos_crdMat2Polygon(env, crdMat, dim);
            geoms[i] = Pol;
        }
        if (npls == 1) {
            GC = geoms[0];
        } else {
            if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, npls)) == NULL) {
                error("Polygons2GC: collection not created");
            }
        }
    } else {

        int nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        for (i=0; i<nErings; i++) {
            Pol = rgeos_Polygons_i_2Polygon(env, pls, VECTOR_ELT(comm, i));
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON,
            geoms, nErings)) == NULL) {
            error("Polygons2GC: collection not created");
        }
    }

    UNPROTECT(pc);
    return(GC);

}
<<<<<<< .mine

GEOSGeom rgeos_SpatialPolygonsGC(SEXP env, SEXP obj) {

    SEXP pls;
    int npls, i, pc=0;
    GEOSGeom *geoms;
    GEOSGeom GC;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(pls = GET_SLOT(obj, install("polygons"))); pc++;
    npls = length(pls);

    geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

    for (i=0; i<npls; i++)
        geoms[i] = rgeos_Polygons2GC(env, VECTOR_ELT(pls, i));

    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, npls)) == NULL) {
        error("rgeos_SpatialPolygonsGC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}=======

// Spatial polygons to fish soup geometry collection (multipoint) 
GEOSGeom rgeos_Polygons2MP(SEXP env, SEXP obj) {

    SEXP pls, crdMat, dim;
    GEOSGeom *geoms;
    GEOSGeom pt, GC;
    int npls, i, ii, j, n, nn=0, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(pls = GET_SLOT(obj, install("Polygons"))); pc++;
    npls = length(pls);

    for (i=0; i<npls; i++) {
        crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
        dim = getAttrib(crdMat, R_DimSymbol);
        nn += (INTEGER_POINTER(dim)[0]-1);
    }

    geoms = (GEOSGeom *) R_alloc((size_t) nn, sizeof(GEOSGeom));

    for (i=0, ii=0; i<npls; i++) {
        crdMat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
        dim = getAttrib(crdMat, R_DimSymbol);
        n = INTEGER_POINTER(dim)[0];
        for (j=0; j<(n-1); j++) {
            pt = rgeos_xy2Pt(env, NUMERIC_POINTER(crdMat)[j],
                NUMERIC_POINTER(crdMat)[j+n]);
            geoms[ii] = pt;
            ii++;
        }
    }

    if ((GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, 
        geoms, nn)) == NULL) {
        error("rgeos_Polygons2MP: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}
>>>>>>> .r63
