checkPolygonsGEOS <- function(obj) {
    if (!is(obj, "Polygons")) 
        stop("not a Polygons object")
    comm <- comment(obj)
    if (!is.null(comm)) {
        comm <- as.integer(unlist(strsplit(comment(obj), " ")))
        comm <- lapply(which(comm == 0), function(y) c(y, which(comm == y)))
    }
    lmat <- .Call("rgeos_PolygonsContain", obj, comm)
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
                Ers <- isn[which(rlmat[isn] %% 2 == 0)]
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

