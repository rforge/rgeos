rgeos_SpatialPolygons2gpcpoly <- function(from) {
	
	if (!inherits(from,"SpatialPolygons"))
		stop("sp does not inherit from SpatialPolygons")
	
	gpcs = list()
	for(i in 1:length(from@polygons)) {
		polys = from@polygons[[i]]
		
		pts = list()
		for(j in 1:length(polys@Polygons)) {
			coords = polys@Polygons[[j]]@coords
			hole = polys@Polygons[[j]]@hole
			l=nrow(coords)
			
			pts[[j]] = list(x=coords[-l,1],y=coords[-l,2],hole=hole)
		}
		
		gpc = new("gpc.poly", pts = pts)
		gpcs[i] = gpc
	}
	
	if (length(gpcs) == 0)
		gpcs = NULL
	if (length(gpcs) == 1)
		gpcs = gpcs[[1]]

	return(gpcs)
}

rgeos_gpcpoly2SpatialPolygons <- function(from) {
	
	if (!is.list(from))
		from = list(from)
		
	res=list()
	for (m in 1:length(from)) {
		gpc = from[[m]]
		
		if (!inherits(gpc,"gpc.poly"))
			stop("gpc does not inherit from gpc.poly")
	
		npoly = length(gpc@pts)
		if (npoly < 1)
			stop("must be at least one polygon")
	
		polylist = list()
		holes = rep(FALSE,npoly)
		for (i in 1:npoly) {
			x=gpc@pts[[i]]$x
			y=gpc@pts[[i]]$y
		
			l=length(x)
			if (x[1]!=x[l] | y[1]!=y[l]) {
				x = c(x,x[1])
				y = c(y,y[1])
			}
			
			polylist[i] = Polygon(cbind(x,y),gpc@pts[[i]]$hole)
			holes[i] = gpc@pts[[i]]$hole
		}
	
		if (sum(holes)==0) {
			res[[m]] = Polygons(polylist,m)
			attr(res[[m]],"comment") = paste(rep(0,npoly),collapse=" ")
			next
		}
		
		templist = list()
		for(i in 1:npoly) {
			templist[i] = Polygons(list(polylist[[i]]),i)
		}
		tempsp = SpatialPolygons(templist)
		
		owners = gContains(tempsp[!holes],tempsp[holes],byid=TRUE)
	
		#make sure all holes belong to only one polygon
		for(i in 1:nrow(owners)) {
			ncontain = sum(owners[i,])
			if (ncontain==0) {
				stop("Invalid polygon(s), hole is not contained within given polygons")
			} else if (ncontain > 1){
				#if a hole is inside multiple polygons then pick the one with the smallest area
				
				areas = sapply(tempsp@polygons[which(!holes)[owners[i,]]], function(x) x@Polygons[[1]]@area )
				iown = which(owners[i,])[which.min(areas)]
				
				owners[i,] = FALSE
				owners[i, iown] = TRUE
			}
		}
	
	
		comm = rep(NA,npoly)
		k=1		
		for(i in which(!holes)) {
			
			comm[i] = 0
			for (j in which(holes)[which(owners[,k])]) {
				comm[j] = i
			}
			k=k+1	
		}
	
		res[[m]] = Polygons(polylist,m)
		attr(res[[m]],"comment") = paste(comm,collapse=" ")
	}
	
	return( SpatialPolygons(res) )
}

setAs("SpatialPolygons", "gpc.poly", rgeos_SpatialPolygons2gpcpoly)
setAs("gpc.poly", "SpatialPolygons", rgeos_gpcpoly2SpatialPolygons)

setAs("SpatialPolygons", "gpc.poly.nohole", rgeos_SpatialPolygons2gpcpoly)
setAs("gpc.poly.nohole", "SpatialPolygons", rgeos_gpcpoly2SpatialPolygons)


areaGPC <- function(x.mat) {
    if(nrow(x.mat) < 3) 
        return(0);   
    x.segmat <- cbind(x.mat, rbind(x.mat[2:nrow(x.mat), ],
         x.mat[1, ]));
    abs(sum(x.segmat[,1] * x.segmat[,4] - x.segmat[,3]
        * x.segmat[,2])) / 2
}





checkHolesGPC <- function(gpclist) {
    stopifnot(length(gpclist) > 0)
    res0 <- .Call("checkHolesGPC", .RGEOS_HANDLE, gpclist, PACKAGE="rgeos")
    if (is.null(res0)) {
        obj <- gpclist
        hls <- sapply(obj, "[[", "hole")
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

