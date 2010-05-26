#include "rgeos.h"
#include <string.h>

SEXP rgeos_double_translate(SEXP env, SEXP obj, SEXP id, SEXP thres) {
    
    SEXP ans, p4s;
    GEOSGeom geom;
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    geom = rgeos_convert_R2geos( env, obj);
    p4s = GET_SLOT(obj, install("proj4string"));
    
    ans = rgeos_convert_geos2R(env, geom, p4s, id, thres); 
    
    return(ans);
}

GEOSGeom rgeos_convert_R2geos(SEXP env, SEXP obj) {
    
    GEOSGeom ans;
    char classbuf[BUFSIZ];
    
    strcpy(classbuf, CHAR( STRING_ELT(GET_CLASS(obj), 0) ));
    
    if ( !strcmp( classbuf,"SpatialPoints") ) {
        ans = rgeos_SpatialPoints2geospoint( env, obj);
    } else if ( !strcmp(classbuf,"SpatialLines") ) {
        ans = rgeos_SpatialLines2geosline( env, obj);
    } else if ( !strcmp(classbuf,"SpatialPolygons") ) {
        ans = rgeos_SpatialPolygons2geospolygon( env, obj);
    } else {
        error("rgeos_convert_R2geos: invalid R class, unable to convert");
    }
    
    return(ans);
} 

// Spatial Points to geometry collection (Multipoints)
GEOSGeom rgeos_SpatialPoints2geospoint(SEXP env, SEXP obj) {
    
    int pc = 0;
    //unsigned int i, n;
    int match, i, j, k, n, nunq, *unique, *unqcnt, *whichid;
    GEOSGeom *geoms, *subgeoms;
    GEOSGeom pt, GC;
    SEXP crds, dim, ids;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    crds = GET_SLOT(obj, install("coords")); 
    dim = getAttrib(crds, install("dim")); 
    n = INTEGER_POINTER(dim)[0];


    
    if ( n == 1 ){ 
        GC = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[0], NUMERIC_POINTER(crds)[1]);
    } else if ( n != 1 ) {
            
        unique  = (int *) R_alloc((size_t) n, sizeof(int));
        unqcnt  = (int *) R_alloc((size_t) n, sizeof(int));
        whichid = (int *) R_alloc((size_t) n, sizeof(int));
        
        PROTECT(ids = VECTOR_ELT( getAttrib(crds, R_DimNamesSymbol), 0 ));pc++;
        
        unique[0] = 0;
        unqcnt[0] = 1;
        whichid[0] = 0;
        nunq = 1;
        
        for (i=1; i<n; i++) {
            match = 0;
            for(j=0; j<nunq; j++) {
                match = !strcmp( CHAR(STRING_ELT(ids, i)), CHAR(STRING_ELT(ids, unique[j])) );
                if (match) break;
            }
            
            if (!match) {
                unique[nunq] = i;
                unqcnt[nunq] = 0;
                nunq++;
            }
            unqcnt[j]++;
            whichid[i] = j;
        }
                
        geoms = (GEOSGeom *) R_alloc((size_t) nunq, sizeof(GEOSGeom));
        
        
        for ( j=0; j<nunq; j++) {

            subgeoms = (GEOSGeom *) R_alloc((size_t) unqcnt[j], sizeof(GEOSGeom));
            k=0;
            for (i=0; i<n; i++) {
                if (whichid[i] == j) {
                    subgeoms[k] = rgeos_xy2Pt(env, NUMERIC_POINTER(crds)[i], NUMERIC_POINTER(crds)[i+n]);
                    k++;
                }
            }
            if (k == 1 ){
                geoms[j] = subgeoms[0];
            } else {
                geoms[j] = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOINT, subgeoms, unqcnt[j]);
            }
            if (geoms[j] == NULL) error("rgeos_SpatialPoints2geospoint: collection not created");
        }

        if (nunq == 1) {
            GC = geoms[0];
        } else {
            GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, nunq);
        }
        if (GC == NULL) error("rgeos_SpatialPoints2geospoint: collection not created");
        
    } else {
        error("rgeos_SpatialPoints2geospoint: invalid dim");
    }
    
    UNPROTECT(pc);
    return(GC);
}

// SpatialLines class to geometry collection
GEOSGeom rgeos_SpatialLines2geosline(SEXP env, SEXP obj) {

    SEXP lines, Lines;
    GEOSGeom *geoms;
    GEOSGeom ls, GC;
    int nlines, i, pc=0;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    PROTECT(lines = GET_SLOT(obj, install("lines"))); pc++;
    nlines = length(lines);

    geoms = (GEOSGeom *) R_alloc((size_t) nlines, sizeof(GEOSGeom));

    for (i=0; i<nlines; i++) {
        Lines = VECTOR_ELT(lines, i);
        geoms[i] = rgeos_Lines2GC(env, Lines);
    }
    
    // If there is only one line collection return multiline not GC
    if (nlines == 1) {
        GC = geoms[0];
    } else {
        GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, nlines);
        if (GC == NULL) error("Lines2GC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}

// Lines class to geometry collection (Multilinestring)
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
    if (nlns == 1) {
        GC = geoms[0];
    } else {
        GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTILINESTRING, geoms, nlns);
    }
    if (GC == NULL) error("Lines2GC: collection not created");

    UNPROTECT(pc);
    return(GC);

}


// Spatial polygons to geometry collection (multipolygon)
GEOSGeom rgeos_SpatialPolygons2geospolygon(SEXP env, SEXP obj) {

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
    
    if (npls == 1) {
        GC = geoms[0];
    } else {
        GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_GEOMETRYCOLLECTION, geoms, npls);
    } 
    if (GC == NULL) error("rgeos_SpatialPolygons2geospolygon: collection not created");


    UNPROTECT(pc);
    return(GC);
}


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
            GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, npls);
        }
        
        if (GC == NULL) error("Polygons2GC: collection not created");
        
    } else {

        int nErings = length(comm);
        geoms = (GEOSGeom *) R_alloc((size_t) nErings, sizeof(GEOSGeom));
        
        for (i=0; i<nErings; i++) {
            geoms[i] = rgeos_Polygons_i_2Polygon(env, pls, VECTOR_ELT(comm, i));
        }
        
        GC = GEOSGeom_createCollection_r(GEOShandle, GEOS_MULTIPOLYGON, geoms, nErings);
        if (GC == NULL) error("Polygons2GC: collection not created");
    }

    UNPROTECT(pc);
    return(GC);
}



GEOSGeom rgeos_Polygons_i_2Polygon(SEXP env, SEXP pls, SEXP vec) {

    GEOSGeom res, pol, hole;
    GEOSGeom *holes;
    SEXP mat, dim;

    int n = length(vec);
    int i, j;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    i = INTEGER_POINTER(vec)[0]-R_OFFSET;

    mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
    dim = getAttrib(mat, R_DimSymbol);
    pol = rgeos_crdMat2LinearRing(env, mat, dim);
    if (n == 1) {
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, NULL, (unsigned int) 0)) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    } else {
        holes = (GEOSGeom *) R_alloc((size_t) (n-1), sizeof(GEOSGeom));
        for (j=1; j<n; j++) {
            i = INTEGER_POINTER(vec)[j]-R_OFFSET;
            mat = GET_SLOT(VECTOR_ELT(pls, i), install("coords"));
            dim = getAttrib(mat, R_DimSymbol);
            hole = rgeos_crdMat2LinearRing(env, mat, dim);
            holes[(j-1)] = hole;
        }
        if ((res = GEOSGeom_createPolygon_r(GEOShandle, pol, holes,
            (unsigned int) (n-1))) == NULL) {
            GEOSGeom_destroy_r(GEOShandle, pol);
            error("rgeos_Polygons_i_2Polygon: Polygon not created");
        }
    }
    return(res);
}



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