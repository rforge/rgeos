#include "rgeos.h"

struct ud {
    int count;
    int *ids;
};

void cb(void *item, void *userdata) {
    struct ud *my_UD;
    my_UD = userdata;
    my_UD->ids[my_UD->count] = item;
    my_UD->count++;
}

static struct ud UD;

SEXP rgeos_poly_findInBox(SEXP env, SEXP pls) {

    GEOSGeom *bbs;
    int npls, i, j, jj, pc=0;
    GEOSGeom GC, bb;
    SEXP pl, bblist;
    GEOSSTRtree *str;
    int *icard, *ids, *oids;

    GEOSContextHandle_t GEOShandle = getContextHandle(env);

    str = (GEOSSTRtree *) GEOSSTRtree_create_r(GEOShandle, (size_t) 10);

    npls = length(pls);
    bbs = (GEOSGeom *) R_alloc((size_t) npls, sizeof(GEOSGeom));
    ids = (int *) R_alloc((size_t) npls, sizeof(int));
    UD.ids = (int *) R_alloc((size_t) npls, sizeof(int));
    oids = (int *) R_alloc((size_t) npls, sizeof(int));

    for (i=0; i<npls; i++) {
        ids[i] = i;
        pl = VECTOR_ELT(pls, i);
        if ((GC = rgeos_Polygons2GC(env, pl)) == NULL) {
            error("rgeos_poly2nb: GC[%d] not created", i);
        }
        if ((bb = GEOSEnvelope_r(GEOShandle, GC)) == NULL) {
            error("rgeos_poly2nb: envelope [%d] not created", i);
        }
        bbs[i] = bb;
        GEOSSTRtree_insert_r(GEOShandle, str, bb, ids[i]);
    }

    icard = (int *) R_alloc((size_t) npls, sizeof(int));
    PROTECT(bblist = NEW_LIST(npls-1)); pc++;

    for (i=0; i<(npls-1); i++) {
        UD.count = 0;
        GEOSSTRtree_query_r(GEOShandle, str, bbs[i],
            (GEOSQueryCallback *) cb, &UD);
        for (j=0, jj=0; j<UD.count; j++) if (UD.ids[j] > i) jj++;
        icard[i] = jj;
        if (icard[i] > 0) SET_VECTOR_ELT(bblist, i, NEW_INTEGER(icard[i]));

        for (j=0, jj=0; j<UD.count; j++) {
            if (icard[i] > 0 && UD.ids[j] > i) {
                oids[jj] = UD.ids[j] + R_OFFSET;
                jj++;
            }
        }
        R_isort(oids, jj);
        for (j=0; j<jj; j++) {
            INTEGER_POINTER(VECTOR_ELT(bblist, i))[j] = oids[j];
        }
    }

    UNPROTECT(pc);
    return(bblist);
}


