checkPolygonsrGEOS <- function(obj) {
    if (!is(x, "Polygons")) 
        stop("not an Polygons object")
    lmat <- .Call("rgeos_PolygonsContain", obj)
    if (is.null(lmat)) {
        return(obj)
    } else {
        n <- dim(lmat)[1]
        containsij <- which(lmat, arr.ind=TRUE)
        
        eRiR <- as.integer(1:n %in% containsij[,2])
        eRiR[containsij[,2]] <- containsij[,1]
        ceRiR <- paste(eRiR, collapse=" ")
        comment(obj) <- ceRiR
        
}

