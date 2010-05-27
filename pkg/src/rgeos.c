#include <math.h>
#include "rgeos.h"

SEXP rgeos_GEOSversion(void) {

    SEXP ans;

    PROTECT(ans=NEW_CHARACTER(1));
    SET_STRING_ELT(ans, 0, COPY_TO_USER_STRING(GEOSversion()));

    UNPROTECT(1);

    return(ans);
}

static void __errorHandler(const char *fmt, ...) {

    char buf[BUFSIZE], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    error(buf);

    return;
  
}

static void __warningHandler(const char *fmt, ...) {

    char buf[BUFSIZE], *p;
    va_list(ap);
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';

    warning(buf);
    
    return;
}

GEOSContextHandle_t getContextHandle(SEXP env) {

    GEOSContextHandle_t r;
    SEXP ptr;

    ptr = findVarInFrame(env, install("GEOSptr"));
    r = R_ExternalPtrAddr(ptr);

    return(r);

}

SEXP rgeos_Init(void) {

    GEOSContextHandle_t r;
    SEXP sxpHandle;

    r = initGEOS_r((GEOSMessageHandler) __warningHandler, (GEOSMessageHandler) __errorHandler);

    sxpHandle = R_MakeExternalPtr((void *) r, mkChar("GEOSContextHandle"), R_NilValue);
    R_RegisterCFinalizerEx(sxpHandle, rgeos_finish_handle, TRUE);
 
    return(sxpHandle);

}

static void rgeos_finish_handle(SEXP ptr) {

    if(!R_ExternalPtrAddr(ptr)) return;
    R_ClearExternalPtr(ptr);
}


SEXP rgeos_finish(SEXP env) {

    GEOSContextHandle_t r;
    SEXP sxpHandle;

    r = getContextHandle(env);

    finishGEOS_r(r);

    sxpHandle = findVarInFrame(env, install("GEOSptr"));

    rgeos_finish_handle(sxpHandle);

    return(R_NilValue);

}



double getScale(SEXP env) {

    double r;
   
    r =  NUMERIC_POINTER( findVarInFrame(env, install("scale")) )[0];

    return(r);
}

double rgeos_round(double val) {
    return java_math_round(val);
}


double makePrecise(double val, double scale) {
    return( rgeos_round(val*scale)/scale );
}

// Symmetric Rounding Algorithm  - equivalent to C99 round()
double sym_round(double val) {
    double n;
    double f = fabs(modf(val, &n));
    if (val >= 0) {
        if (f < 0.5) {
            return floor(val);
        } else if (f > 0.5) {
            return ceil(val);
        } else {
            return (n + 1.0);
        }
    } else {
        if (f < 0.5) {
            return ceil(val);
        } else if (f > 0.5) {
            return floor(val);
        } else {
            return (n - 1.0);
        }
    }
}


// Asymmetric Rounding Algorithm  - equivalent to Java Math.round()
double java_math_round(double val) {
    double n;
    double f = fabs(modf(val, &n));

    if (val >= 0) {
        if (f < 0.5) {
            return floor(val);
        } else if (f > 0.5) {
            return ceil(val);
        } else {
            return (n + 1.0);
        }
    } else {
        if (f < 0.5) {
            return ceil(val);
        } else if (f > 0.5) {
            return floor(val);
        } else {
            return n;
        }
    }
} 

// Implementation of rint() 
double rint_vc(double val) {
    double n;
    double f=fabs(modf(val,&n));
    if (val>=0) {
        if (f<0.5) {
            return floor(val);
        } else if (f>0.5) {
            return ceil(val);
        } else {
            return(floor(n/2)==n/2)?n:n+1.0;
        }
    } else {
        if (f<0.5) {
            return ceil(val);
        } else if (f>0.5) {
            return floor(val);
        } else {
            return(floor(n/2)==n/2)?n:n-1.0;
        }
    }
}

void printCoordSeq(SEXP env, GEOSCoordSeq s) {
    int i,n;
    double x,y;
    
    GEOSContextHandle_t GEOShandle = getContextHandle(env);
    
    GEOSCoordSeq_getSize_r(GEOShandle, s, &n);
    
    for (i=0; i<n; i++) {
        
        GEOSCoordSeq_getX_r(GEOShandle, s, i, &x);
        GEOSCoordSeq_getY_r(GEOShandle, s, i, &y);
        
        Rprintf("%f %f\n",x,y);
    }
    
    
}



