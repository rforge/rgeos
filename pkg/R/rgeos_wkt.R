
#TODO - this should probably be more robust
isWKT = function( s ) {
    
    s = str_trim(s)
    validWKT = c("^POINT", "^LINESTRING", "^LINEARRING", "^POLYGON", "^MULTIPOINT", 
                 "^MULTILINESTRING", "^MULTIPOLYGON", "^GEOMETRYCOLLECTION")
    
    return(any( sapply(validWKT, function(x) str_detect(s,x)) ))
}


readWKT = function( text, id = NULL, p4s = NULL, threshold=0) {
    
    wkts = cleanWKT(text)
    
    m =  str_match_all(wkts, "POINT|LINESTRING|LINEARRING|POLYGON|MULTIPOINT|MULTILINESTRING|MULTIPOLYGON")   
    ngeoms =  sapply(m, function(x) dim(x)[1] )
    
    if(is.null(id)) id = 1:sum(ngeoms)

    # if the number of ids doesn't take into account geometry collections create subids
    if( length(id) == length(wkts)) {
        newid = c()
        for( i in 1:length(wkts) ) {
            if(ngeoms[i] == 1) {
                newid = c(newid, id[i])
            } else {
                newid = c(newid, paste(id[i],1:ngeoms[i],sep=".") )
            }
        }
        id = newid
    }

    if( length(id) != sum(ngeoms) )
        stop("number of WKT geometries does not match number of ids")        

    p4s = checkP4S(p4s)
    
    threshold=as.double(threshold)
    id = as.character(id)
        
    res = list()
    for(i in 1:length(wkts) ) {
        ids = id[ sum( ngeoms[1:i] )-ngeoms[i]+1:ngeoms[i] ]
        res[[i]] <- .Call("rgeos_readWKT", .RGEOS_HANDLE, wkts[i], p4s, ids, threshold, PACKAGE="rgeos")
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