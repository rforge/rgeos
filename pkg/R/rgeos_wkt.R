
#TODO - this should probably be more robust
isWKT = function( s ) {
    
    s = str_trim(s)
    validWKT = c("^POINT", "^LINESTRING", "^LINEARRING", "^POLYGON", "^MULTIPOINT", 
                 "^MULTILINESTRING", "^MULTIPOLYGON", "^GEOMETRYCOLLECTION")
    
    return(any( sapply(validWKT, function(x) str_detect(s,x)) ))
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

    wkts = lapply( strsplit(text,'\n'), str_trim)
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

readWKT = function( text, id = NULL, p4s = NULL, threshold=0) {
    
    wkts = cleanWKT(text)
    
    if(is.null(id)) id = 1:length(wkts)

    if( length(wkts) != length(id) )
        stop("number of WKT strings does not match number of ids")        

    p4s = checkP4S(p4s)
    
    threshold=as.double(threshold)
    id = as.character(id)
        
    res = list()
    for(i in 1:length(wkts) ) {
        res[[i]] <- .Call("rgeos_readWKT", .RGEOS_HANDLE, wkts[i], p4s, id[i], threshold, PACKAGE="rgeos")
    }
    if(length(res) == 1) {
        res <- res[[1]]
    }
    
    return( res )
}


# TODO - could be done more efficiently with regexs
cleanWKT = function( text ) {
    
    text = str_replace(str_trim(text),"\n","")

    openPos  = c( gregexpr("\\(", text)[[1]], nchar(text)+1)
    closePos = c( gregexpr("\\)", text)[[1]], nchar(text)+1)

    i = 1
    j = 1
    splitPos = c()
    count = 0
    while ( i < length(openPos) | j < length(closePos) ) {
        if (openPos[i] < closePos[j]) {
            count = count+1
            i = i+1
        } else {
            count = count-1
            j = j+1
        }
    
        if (count < 0) stop("invalid WKT text, unbalanced parenthesis")
        
        if (count == 0) splitPos = c(splitPos, closePos[j-1])
    }
    
    if (count != 0 ) stop("invalid WKT text, unbalanced parenthesis")
    
    startPos = c(1, splitPos[-length(splitPos)]+1)
    
    rep = str_trim( substring(text,startPos,splitPos) )
    
    validWKTs = sapply(rep,isWKT)
    if ( !all(validWKTs) ) {
        warning(paste("cleanWKT: following WKT strings are invalid, ignoring.\n",
                      paste( rep[validWKTs],collapse="\n" ) ))
    }
    
    return( rep[validWKTs] )
}