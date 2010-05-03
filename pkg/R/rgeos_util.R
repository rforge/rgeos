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
