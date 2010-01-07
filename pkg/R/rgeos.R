checkPolygonsGEOS <- function(obj) {
    if (!is(obj, "Polygons")) 
        stop("not a Polygons object")
    lmat <- .Call("rgeos_PolygonsContain", obj, PACKAGE="rgeos")
    if (is.null(lmat)) {
        Pls <- slot(obj, "Polygons")
        hls <- sapply(Pls, function(x) slot(x, "hole"))
        if (any(hls)) {
            for (i in 1:length(Pls)) {
                if (hls[i]) {
                    Pl <- Pls[[i]]
                    crds <- slot(Pl, "coords")
                    crds <- crds[nrow(crds):1,]
                    Pls[[i]] <- Polygon(crds, hole=FALSE)
                }
            }
            slot(obj, "Polygons") <- Pls
        }
        comment(obj) <- paste(rep(0, length(hls)), collapse=" ")
        return(obj)
    }
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

SpatialLinesIntersections <- function(SL1, SL2) {
    stopifnot(is(SL1, "SpatialLines"))
    stopifnot(is(SL2, "SpatialLines"))
    sl1 <- slot(SL1, "lines")
    sl2 <- slot(SL2, "lines")
    p4s <- proj4string(SL1)
    if (!is.na(p4s) && p4s != proj4string(SL2)) 
        warning("Object coordinate reference systems differ")
    res <- NULL
    id1 <- NULL
    id2 <- NULL
    for (i in seq(along=sl1)) {
        sl1i <- sl1[[i]]
        for (j in seq(along=sl2)) {
            zzz <- LinesIntersections(sl1i, sl2[[j]])
            if (!is.null(zzz)) {
                res <- rbind(res, zzz)
                id1 <- c(id1, rep(slot(sl1i, "ID"), dim(zzz)[1]))
                id2 <- c(id2, rep(slot(sl2[[j]], "ID"), dim(zzz)[1]))
            }
        }
    }
    if (is.null(res)) return(res)
    df <- data.frame(id1, id2)
    SpatialPointsDataFrame(res, proj4string=CRS(p4s), data=df)
}

LinesIntersections <- function(Lobj1, Lobj2) {
    stopifnot(is(Lobj1, "Lines"))
    stopifnot(is(Lobj2, "Lines"))
    .Call("rgeos_Lines_intersection", Lobj1, Lobj2, PACKAGE="rgeos")
}

PolygonsIntersections <- function(Pobj1, Pobj2) {
    stopifnot(is(Pobj1, "Polygons"))
    stopifnot(is(Pobj2, "Polygons"))
    .Call("rgeos_Polygons_intersection", Pobj1, Pobj2, PACKAGE="rgeos")
}

