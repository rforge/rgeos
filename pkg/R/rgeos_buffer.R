
# width - double, quadsegs - int, joinStyle - int, mitreLimit - double, leftSide - int

RGEOSBuffer = function(spgeom, width, quadsegs) {
    
    id = as.character(1:10)
    
    x = .Call("rgeos_buffer", .RGEOS_HANDLE, spgeom, id, 0.0, width, as.integer(quadsegs), PACKAGE="rgeos")
    return(x)
}

RGEOSBufferWithStyle = function(spgeom, width, quadsegs, endCapStyle, joinStyle, mitreLimit) {
    
    GEOSCapStyles = c("ROUND","FLAT","SQUARE")
    GEOSJoinStyles = c("ROUND","MITRE","BEVEL")
    
    
    
}

RGEOSSingleSidedBuffer = function(spgeom, width, quadsegs, joinStyle, mitreLimit, leftSide) {
    
    GEOSCapStyles = c("ROUND","FLAT","SQUARE")
    GEOSJoinStyles = c("ROUND","MITRE","BEVEL")
    
    
}
