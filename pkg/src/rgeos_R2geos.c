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
