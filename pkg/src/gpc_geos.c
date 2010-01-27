#include "rgeos.h"

SEXP SymDiffGpcGEOS(SEXP A, SEXP B) {

    GEOSGeom GCA, GCB, GCC;

    GCA = GCPPtsGC(A);
    GCB = GCPPtsGC(B);

    

}

SEXP UnionGpcGEOS(SEXP A, SEXP B) {

    GEOSGeom GCA, GCB, GCC;

    GCA = GCPPtsGC(A);
    GCB = GCPPtsGC(B);


}

SEXP IntersectGpcGEOS(SEXP A, SEXP B) {

    GEOSGeom GCA, GCB, GCC;

    GCA = GCPPtsGC(A);
    GCB = GCPPtsGC(B);


}

SEXP checkHolesGPC(SEXP A) {

    GEOSGeom GC;

    GC = GCPPtsGC(A);

    return(GC_Contains(GC));

}

SEXP GCpolysGPCpts(GEOSGeom GC) {

}

GEOSGeom GPCptPolygon(SEXP obj) {

    GEOSGeom g1, p1;

    g1 = GPCpt2LinearRing(obj);

    if ((p1 = GEOSGeom_createPolygon(g1, NULL, (unsigned int) 0)) == NULL) {
        GEOSGeom_destroy(g1);
        error("GPCptPolygon: Polygon not created");
    }

    return(p1);

}

GEOSGeom GPCpt2LinearRing(SEXP obj) {

    GEOSCoordSeq s;

    s = GPCpt2CoordSeq(obj);

    GEOSGeom gl;
    if ((gl = GEOSGeom_createLinearRing(s)) == NULL) {
        GEOSGeom_destroy(gl);
        error("GPCpt2LinearRing: linearRing not created");
    }
    if ((int) GEOSisValid(gl) == 1) {
        if (GEOSNormalize(gl) == -1)
            warning("GPCpt2LinearRing: normalization failure");
    } else {
        warning("GPCpt2LinearRing: validity failure");
    }


}

GEOSCoordSeq GPCpt2CoordSeq(SEXP obj) {

    unsigned int i, n, m;
    n = (unsigned int) length(VECTOR_ELT(obj, 0));
    m = 2;

    GEOSCoordSeq s;

    s = GEOSCoordSeq_create(n, m);

    for(i=0; i<n; i++) {
        if (GEOSCoordSeq_setX(s, i, 
            NUMERIC_POINTER(VECTOR_ELT(obj, 0))[i]) == 0) {
            GEOSCoordSeq_destroy(s);
            error("GPCpt2CoordSeq: X not set for %d", i);
        }
        if (GEOSCoordSeq_setY(s, i, 
            NUMERIC_POINTER(VECTOR_ELT(obj, 1))[i]) == 0) {
            GEOSCoordSeq_destroy(s);
            error("GPCpt2CoordSeq: Y not set for %d", i);
        }
    }

    return(s);

}


GEOSGeom GCPPtsGC(SEXP pls) {

    SEXP comm;
    GEOSGeom *geoms;
    GEOSGeom Pol, GC;
    int npls, i, pc=0;

    npls = length(pls);

    PROTECT(comm = SP_PREFIX(comment2comm)(pls)); pc++;

    if (comm == R_NilValue) {

        geoms = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));

        for (i=0; i<npls; i++) {
            Pol = GPCptPolygon(VECTOR_ELT(pls, i));
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, geoms, 
            npls)) == NULL) {
            error("GCPPtsGC: collection not created");
        }
    } else {

        int nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        for (i=0; i<nErings; i++) {
            Pol = GPCpt_i_Polygon(pls, VECTOR_ELT(comm, i));
            geoms[i] = Pol;
        }
        if ((GC = GEOSGeom_createCollection(GEOS_MULTIPOLYGON, geoms, 
            nErings)) == NULL) {
            error("GCPPtsGC: collection not created");
        }
    }

    UNPROTECT(pc);
    return(GC);

}

GEOSGeom GPCpt_i_Polygon(SEXP pls, SEXP vec) {

    GEOSGeom res, pol, hole;
    GEOSGeom *holes;

    int n = length(vec);
    int i, j;

    i = INTEGER_POINTER(vec)[0]-R_OFFSET;

    pol = GPCptPolygon(VECTOR_ELT(pls, i));
    if (n == 1) {
        if ((res = GEOSGeom_createPolygon(pol, NULL, (unsigned int) 0))
            == NULL) {
            GEOSGeom_destroy(pol);
            error("GPCpt_i_Polygon: Polygon not created");
        }
    } else {
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            hole = GPCptPolygon(VECTOR_ELT(pls, i));
            holes[(j-1)] = hole;
        }
        if ((res = GEOSGeom_createPolygon(pol, holes, (unsigned int) (n-1)))
            == NULL) {
            GEOSGeom_destroy(pol);
            error("GPCpt_i_Polygon: Polygon not created");
        }
    }
    return(res);
}


