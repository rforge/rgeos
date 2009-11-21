checkPolygonsGEOS <- function(obj) {
    if (!is(x, "Polygons")) 
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
        o <- o-1
        for (oo in o:1) {
            if (oo %% 2 == 0) {
                ids <- which(colSums(l[[oo]]) == 1)
                lmat[,ids] <- FALSE
            } else {
            }
        }

    }
        {

        rlmat <- rowSums(lmat)
        clmat <- colSums(lmat)
        justExternalRings <- which(rlmat == 0 & clmat == 0)
        apparentExternalRingsRoots <- which(rlmat > 0 & clmat == 0)
        ExternalRings_IR <- logical(length(apparentExternalRingsRoots))
        for (i in 1:length(apparentExternalRingsRoots)) {
            Iri <- which(lmat[apparentExternalRingsRoots[i],])
            ExternalRings_IR[i] <- all(colSums(lmat[,Iri]) == 1)
        }
        if (any(!ExternalRings_IR)) {
            for (i in apparentExternalRingsRoots[!ExternalRings_IR]) {
                Iri <- lmat[i,]
                fin <- TRUE
                Ir <- TRUE
                while (fin) {
                    rS <- rowSums(lmat[Iri,Iri])
print(rS)
                    wi <- which(Iri)
print(wi)
                    if (sum(rS > 0)) {
                        id <- wi[which.max(rS)]
cat("Ir", Ir, "id", id, "rS", rS, "\n")
                        if (Ir) {
                            lmat[id,] <- FALSE
                            which(lmat, arr.ind=TRUE)
                            Ir <- FALSE
                        } else {
                            Iri[id] <- FALSE
                            Ir <- TRUE
                        }
                    } else {
                        fin <- FALSE
                    }
                }
            }
        }
    } 
    containsij <- which(lmat, arr.ind=TRUE)
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

