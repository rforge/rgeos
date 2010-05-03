str_stripws = function(str) {
    str = sub('\\s+$', '', str, perl = TRUE)
    str = sub('^\\s+', '', str, perl = TRUE)
    return(str)
}

SP2WKT = function( sp,belongs=NULL ) {

    if (!inherits(sp, "SpatialPolygons")) 
        stop("not a SpatialPolygons object")
    
    if( is.null(belongs) ) 
        belongs = list( seq(1,length(sp@polygons)) )
    
    res = rep('',length(belongs))
    for( b in 1:length(belongs) ) {
        wkt = ""
        poly_wkt = rep('',length(belongs[[b]]) )
        
        i=1
        for( j in belongs[[b]] ) {
            
            rings = c()
            for(p in sp@polygons[[j]]@Polygons) {
                
                ring_wkt = paste('(', paste( apply(apply(p@coords,1,format,nsmall=16),2,paste,collapse=' '), collapse=', ' ), ')',sep='')
            
                if(p@hole) {
                    rings = c(rings, ring_wkt)
                }
                else {
                    rings = c(ring_wkt, rings)
                }
            }
            poly_wkt[i] = paste( "(", paste(rings,collapse=', '), ")",sep='' )
            i=i+1
        }
        if( i==2 ){
            res[b] = paste("POLYGON", poly_wkt[1]) 
        }
        else{
            res[b] = paste("MULTIPOLYGON (" ,paste(poly_wkt,collapse=', '), ")",sep='')
        }
    }
    
    return(res)
}

WKT2SP = function( text,id=NULL,threshold=0.0 ) {

    wkts = lapply( strsplit(text,'\n'), str_stripws)
    if(is.null(id))
        id = 1:length( wkts[[1]] )
    
    if( length(wkts[[1]]) != length(id) )
        stop("number of WKT strings does not match number of ids")
    
    threshold=as.double(threshold)
    id = as.character(id)
        
    res = list()
    j=1
    for(i in 1:length( wkts[[1]] ) ) {
        if(wkts[[1]][i] != "") {
            res[[j]] <- .Call("rgeos_wkt2sp", .RGEOS_HANDLE, wkts[[1]][i], id[i], threshold,PACKAGE="rgeos")
            j=j+1
        }
    }
    return( SpatialPolygons(res) )
}