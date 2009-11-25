checkPolygonsGEOS <- function(obj) {
    if (!is(obj, "Polygons")) 
        stop("not a Polygons object")
    lmat <- .Call("rgeos_PolygonsContain", obj)
    if (is.null(lmat)) return(obj)
    if (sum(lmat %*% lmat) > 0) {
        o <- 1
        l <- list()
        l[[1]] <- lmat
        while(sum(l[[o]]) > 0) {
            l[[o+1]] <- l[[o]] %*% lmat
            o <- o+1
        }
        rlmat <- rowSums(lmat)
        clmat <- colSums(lmat)
        containers <- which(rlmat > 0 & clmat == 0)
        if (o >= 3) lmat1 <- lmat - (l[[2]] - l[[3]])
        for (i in seq(along=containers)) {
            isn <- which(lmat[containers[i],] > 0)
            if (any(colSums(l[[2]])[isn] > 0)) {
                if (o %% 2 == 0) {
                    Ers <- isn[which(rlmat[isn] %% 2 != 0)]
                } else {
                    Ers <- isn[which(rlmat[isn] %% 2 == 0)]
                }
                lmat1[, Ers] <- FALSE
            }
        }
        lmat <- lmat1
    } 
    containsij <- which(lmat == 1, arr.ind=TRUE)
    pls <- slot(obj, "Polygons")
    for (i in containsij[,2]) {
        pl <- pls[[i]]
        if (!slot(pl, "hole")) {
            pl <- Polygon(slot(pl, "coords"), hole=TRUE)
            pls[[i]] <- pl
        }
    }
    slot(obj, "Polygons") <- pls
    n <- dim(lmat)[1]
    eRiR <- as.integer(1:n %in% containsij[,2])
    eRiR[containsij[,2]] <- containsij[,1]
    ceRiR <- paste(eRiR, collapse=" ")
    comment(obj) <- ceRiR
    return(obj)
}

comment2comm <- function(str) {
    if (is.null(str)) return(str)
    res <- as.integer(unlist(strsplit(str, " ")))
    res <- lapply(which(res == 0), function(y) c(y, which(res == y)))
    res
}
