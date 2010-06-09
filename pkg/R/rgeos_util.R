getScale <- function() {
    return( mget("scale",.RGEOS_HANDLE)$scale )
}

setScale <- function(scale=100000000) {
    
    maxPreciseValue <- 9007199254740992.0
    
    if(scale > maxPreciseValue){
        stop("Specified scale is larger than maximum allowed")
    }
    
    assign("scale",scale,envir=.RGEOS_HANDLE)
}

checkP4S = function(proj4string) {
    
    if ( is.null(proj4string) )
        proj4string = CRS(as.character(NA))

    if( is.character(proj4string))
        proj4string = CRS(proj4string) 
    
    if (length(proj4string) != 1)
        stop("proj4string must be of length 1")
    
    if ( class(proj4string) != "CRS") {
        stop("proj4string has invalid class")
    }
    
    return( proj4string )
}

extractIDs = function(obj) {
    
    if ( inherits(obj,"SpatialPoints") ) {
        ids = unique( rownames(obj@coords) )
    } else if ( inherits(obj,"SpatialLines")  ) {
        ids = sapply(obj@lines, function(x) {x@ID})
    } else if ( inherits(obj,"SpatialPolygons") ) {
        ids = sapply(obj@polygons, function(x) {x@ID})
    } else {
        stop("Unknown object class")
    }
    
    return(ids)
}

doubletranslate = function(obj) {
    
    ids = extractIDs(obj)
    
    x = .Call("rgeos_double_translate", .RGEOS_HANDLE, obj, ids, 0, PACKAGE="rgeos")
    return(x)
}

groupID = function(spgeom, ids) {
    
    if (length(row.names(spgeom)) != length(ids)) 
        stop("length of ids does not match number of geometries")
    
    newids = unique(ids)
    
    if (length(row.names(spgeom)) == 1 || length(row.names(spgeom)) == length(newids) || 
        inherits(spgeom,"SpatialPoints") ) {
        
        row.names(spgeom) <- ids
        return(spgeom)
    }
    
    
    if ( inherits(spgeom,"SpatialLines")  ) {
        
        lineslist = list()
        k=1
        for (curid in newids) {
            
            linelist = list()
            l = 1
            for ( i in which(ids == curid) ){
                    
                L = length(spgeom@lines[[i]]@Lines)
                linelist[l:(l+L-1)] = spgeom@lines[[i]]@Lines
                l=l+L
            }
            
            lineslist[[k]] = Lines(linelist, curid)
            k=k+1
        }

        ans = SpatialLines(lineslist,proj4string = spgeom@proj4string)
        
    } else if ( inherits(spgeom,"SpatialPolygons") ) {
        
        polyslist = list()
        k=1
        for (curid in newids) {
            
            comment = c()
            polylist = list()
            l = 1
            for ( i in which(ids == curid) ){
                
                L = length(spgeom@polygons[[i]]@Polygons)
                
                comm = attr(spgeom@polygons[[i]],"comment")
                if (is.null(comm)) comm = rep(0,L)
                else comm = as.integer( strsplit(comm," ")[[1]] )
                comm[comm!=0] = comm[comm!=0] + l-1 
                comment = c(comment, comm)
                    
                polylist[l:(l+L-1)] = spgeom@polygons[[i]]@Polygons
                l=l+L
            }
            
            polyslist[[k]] = Polygons(polylist, curid)
            attr(polyslist[[k]],"comment") = paste(comment, collapse=" ")
            k=k+1
        }

        ans = SpatialPolygons(polyslist,proj4string = spgeom@proj4string)
        
    } else {
        stop("Unknown object class")
    }
    
    return(ans)
}