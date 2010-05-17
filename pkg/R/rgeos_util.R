getScale <- function() {
    return( mget("scale",.RGEOS_HANDLE)$scale )
}

setScale <- function(scale) {
    
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
    