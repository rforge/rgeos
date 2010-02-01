## gpclib:  General Polygon Clipping library for R
## Copyright (C) 2003-2004 Roger D. Peng <rpeng@jhsph.edu>


## R functions for using GPC library and manipulating polygons

## Compute the area of each polygon in the polygon set contained in
## `object'.  

areaGPC <- function(x.mat) {
    if(nrow(x.mat) < 3) 
        return(0);   
    x.segmat <- cbind(x.mat, rbind(x.mat[2:nrow(x.mat), ],
         x.mat[1, ]));
    abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3]
        * x.segmat[,2])) / 2
}

