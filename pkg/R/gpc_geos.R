SymDiffGpcGEOS <- function(gpclist1, gpclist2) {
    stopifnot(length(gpclist1) > 0)
    stopifnot(length(gpclist2) > 0)
    gpclist1 <- checkHolesGPC(gpclist1)
    gpclist2 <- checkHolesGPC(gpclist2)
    .Call("SymDiffGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


UnionGpcGEOS <- function(gpclist1, gpclist2) {
    stopifnot(length(gpclist1) > 0)
    stopifnot(length(gpclist2) > 0)
    gpclist1 <- checkHolesGPC(gpclist1)
    gpclist2 <- checkHolesGPC(gpclist2)
    .Call("UnionGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


IntersectGpcGEOS <- function(gpclist1, gpclist2) {
    stopifnot(length(gpclist1) > 0)
    stopifnot(length(gpclist2) > 0)
    gpclist1 <- checkHolesGPC(gpclist1)
    gpclist2 <- checkHolesGPC(gpclist2)
    .Call("IntersectGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


checkHolesGPC <- function(gpclist) {
    stopifnot(length(gpclist) > 0)
    res0 <- .Call("checkHolesGPC", gpclist, PACKAGE="rgeos")
    if (is.null(res0)) {
        obj <- gpclist
        hls <- sapply(obj, , "[[", "hole")
        if (any(hls)) {
            for (i in seq(along=obj)) 
                if (hls[i]) obj[[i]][["hole"]] <- FALSE
        }
        comment(obj) <- paste(rep(0, length(gpclist)), collapse=" ")
        return(obj)
    }
    lmat <- res0[[1]]
# handle equals deletion
    idmat <- res0[[2]]
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
        areas <- sapply(gpclist, function(p) areaGPC(cbind(p$x, p$y)))
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
    obj <- gpclist
    for (i in containsij[,2]) {
        pl <- obj[[i]]
        if (!pl[["hole"]]) {
            pl[["hole"]] <- TRUE
            obj[[i]] <- pl
        }
    }
    n <- dim(lmat)[1]
    eRiR <- as.integer(1:n %in% containsij[,2])
    eRiR[containsij[,2]] <- containsij[,1]
    ceRiR <- paste(eRiR, collapse=" ")
    comment(obj) <- ceRiR
    return(obj)
}

