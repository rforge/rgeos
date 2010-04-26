checkPolygonsGEOS <- function(obj) {
    if (!is(obj, "Polygons")) 
        stop("not a Polygons object")
    res0 <- .Call("rgeos_PolygonsContain", .RGEOS_HANDLE, obj, PACKAGE="rgeos")
    lmat <- res0[[1]]
    idmat <- res0[[2]]
# handle equals deletion
    if (is.null(res0)) {
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
    if (any(idmat == 1)) {
        idents <- which(idmat == 1, arr.ind = TRUE)
        idents2 <- which(idmat %*% idmat == 1, arr.ind=TRUE)
        if (all(idents2[,1] == idents2[,2])) {
            done <- NULL
            for (i in 1:nrow(idents)) {
                j <- idents[i,1]
                if (is.na(match(j, done))) {
                    jj <- match(j, idents[,2])
                    if (!is.na(jj)) {
                        done <- c(done, c(j, idents[i,2]))
                        lmat[j, idents[i,2]] <- FALSE
                    } else 
                        warning("Odd cycles: no adjustment for equal polygons")
                }
            }
        } else warning("Odd cycles: no adjustment for equal polygons")
    }
    containsij <- which(lmat == 1, arr.ind=TRUE)
    if (sum(lmat %*% lmat) > 0) {
        areas <- sapply(slot(obj, "Polygons"), slot, "area")
        ss <- split(containsij[,1], containsij[,2])
        island <- sapply(ss, function(x) (length(x) %% 2) == 0)
        islands <- as.integer(names(island)[island])
        sss <- lapply(ss, function(x) {
            if (length(x) == 1) return(x)
            x[which.min(areas[x])]
            })
        sss <- sss[-match(islands, names(sss))]
        res <- NULL
        for (i in seq(along=sss))
           res <- rbind(res, c(sss[[i]], as.integer(names(sss)[i])))
        colnames(res) <- c("row", "col")
        containsij <- res
    }
    pls <- slot(obj, "Polygons")
    for (i in containsij[,2]) {
        pl <- pls[[i]]
        if (!slot(pl, "hole")) {
            pl <- Polygon(slot(pl, "coords"), hole=TRUE)
            pls[[i]] <- pl
        }
    }
#    slot(obj, "Polygons") <- pls
    oobj <- Polygons(pls, ID=slot(obj, "ID"))
    n <- dim(lmat)[1]
    eRiR <- as.integer(1:n %in% containsij[,2])
    eRiR[containsij[,2]] <- containsij[,1]
    ceRiR <- paste(eRiR, collapse=" ")
    comment(oobj) <- ceRiR
    return(oobj)
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
    .Call("rgeos_Lines_intersection", .RGEOS_HANDLE, Lobj1, Lobj2, PACKAGE="rgeos")
}

PolygonsIntersections <- function(Pobj1, Pobj2) {
    stopifnot(is(Pobj1, "Polygons"))
    stopifnot(is(Pobj2, "Polygons"))
    .Call("rgeos_Polygons_intersection", .RGEOS_HANDLE, Pobj1, Pobj2, PACKAGE="rgeos")
}

poly2nbGEOS(pl, row.names=NULL, snap=NULL, queen=NULL, nn=NULL) {
    if (!is.null(snap)) warning("snap ignored in GEOS")
    if (!is.null(queen)) warning("snap ignored in GEOS")
    stopifnot(is(pl, "SpatialPolygons"))
    n <- length(slot(pl, "polygons"))
    stopifnot(is.matrix(nn))
    stopifnot(is.integer(nn))
    stopifnot(nrow(nn) == n)
    if (!is.null(row.names)) {
        stopifnot(length(row.names) == n)
        stopifnot(is.character(row.names))
    }
    .Call("rgeos_poly2nb", .RGEOS_HANDLE, pl, row.names, nn)
}
