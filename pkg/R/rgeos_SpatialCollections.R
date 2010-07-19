
setClassUnion("SpatialPointsNULL", c("SpatialPoints", "NULL")) 
setClassUnion("SpatialLinesNULL", c("SpatialLines", "NULL")) 
setClassUnion("SpatialRingsNULL", c("SpatialRings", "NULL")) 
setClassUnion("SpatialPolygonsNULL", c("SpatialPolygons", "NULL")) 

setClass("SpatialCollections",
    representation("Spatial", pointobj = "SpatialPointsNULL", 
							  lineobj = "SpatialLinesNULL",
							  ringobj = "SpatialRingsNULL",
							  polyobj = "SpatialPolygonsNULL",
							  plotOrder = "integer"),
    
    prototype = list(bbox = matrix( rep(NA, 2), 2, 2, dimnames = list(c("x","y"), c("min","max"))),
                     proj4string = CRS(as.character(NA)),
                     pointobj = NULL,
					 lineobj = NULL,
					 ringobj = NULL,
					 polyobj = NULL ),
    
    validity = function(object) {
		if (!is.null(object@pointobj)) validObject(object@pointobj)
		if (!is.null(object@lineobj)) validObject(object@lineobj)
		if (!is.null(object@ringobj)) validObject(object@ringobj)
		if (!is.null(object@polyobj)) validObject(object@polyobj)
		
		return(TRUE)
    }
)

SpatialCollections <- function( points = NULL, lines = NULL,
								rings = NULL, polygons = NULL,
								plotOrder = c(4,3,2,1),
								proj4string=CRS(as.character(NA))) {

	plotOrder=as.integer(plotOrder)
	stopifnot(is.integer(plotOrder))
	stopifnot(length(plotOrder) == 4)
	
	stopifnot(inherits(points,"SpatialPoints") | is.null(points))
	stopifnot(inherits(lines,"SpatialLines") | is.null(lines))
	stopifnot(inherits(rings,"SpatialRings") | is.null(rings))
	stopifnot(inherits(polygons,"SpatialPolygons") | is.null(polygons))
	stopifnot(is(proj4string, "CRS"))
	
	bb = c()
	if (!is.null(points)) bb = rbind(bb,c(bbox(points)))
	if (!is.null(lines)) bb = rbind(bb,c(bbox(lines)))
	if (!is.null(rings)) bb = rbind(bb,c(bbox(rings)))
	if (!is.null(polygons)) bb = rbind(bb,c(bbox(polygons)))
	
	if (length(bb) == 0) {
		bbox = matrix( rep(NA, 2), 2, 2)
	} else {
		bbox = matrix(c(min(bb[,1]), min(bb[,2]), max(bb[,3]), max(bb[,4])), 2, 2)
	}
	dimnames(bbox) = list(c("x", "y"), c("min", "max"))
	
	
	
	Sp <- new("Spatial", bbox = bbox, proj4string=proj4string)	
    res <- new("SpatialCollections", Sp, plotOrder=plotOrder, pointobj=points, 
				lineobj=lines, ringobj=rings, polyobj=polygons)
	#validObject(res)

    return(res)
}

plotSpatialCollections <- function(SC, 
								   pointopt = list(), lineopt = list(),
								   ringopt = list(), polyopt = list(col=NA),
								   pch = 3, cex = 1, bg = 1,
								   col = 1, lwd = 1, lty=1,
								   border = par("fg"), xpd = NULL, 
								   density = NULL, angle = 45, pbg=NULL,
								   xlim = NULL, ylim = NULL,
								   add = FALSE, axes = FALSE, ...,
	  							   setParUsrBB=FALSE) {
		
	first = TRUE
	for (i in order(SC@plotOrder)) {
		if (first) {
			first = FALSE
		} else {
			add = TRUE
		}
		
		if (i == 1 & !is.null(SC@pointobj)) { # plot points
			print("point")
			ptpch = pch
			ptcex = cex
			ptcol = col
			ptlwd = lwd
			ptbg  = bg
			
			if (!is.null(pointopt[["pch"]])) ptpch = pointopt[["pch"]]
			if (!is.null(pointopt[["cex"]])) ptcex = pointopt[["cex"]]
			if (!is.null(pointopt[["col"]])) ptcol = pointopt[["col"]]
			if (!is.null(pointopt[["lwd"]])) ptlwd = pointopt[["lwd"]]
			if (!is.null(pointopt[["bg"]]))  ptbg  = pointopt[["bg"]]
			
			plot(SC@pointobj, pch=ptpch, axes=axes, add=add, xlim=xlim, ylim=ylim, 
				 cex=ptcex, col=ptcol, lwd=ptlwd, bg=ptbg, ..., setParUsrBB=setParUsrBB)
			
		} else if (i == 2 & !is.null(SC@lineobj)) { #plot lines
			print("line")
			lcol = col
			llwd = lwd
			llty = lty
			
			if (!is.null(lineopt[["col"]])) lcol = lineopt[["col"]]
			if (!is.null(lineopt[["lwd"]])) llwd = lineopt[["lwd"]]
			if (!is.null(lineopt[["lty"]])) llty = lineopt[["lty"]]
			
			plot(SC@lineobj, xlim=xlim, ylim=ylim, col=lcol, lwd=llwd, lty=llty,
				 add=add, axes=axes, ..., setParUsrBB=setParUsrBB)
			
		} else if (i == 3 & !is.null(SC@ringobj)) { #plot rings
			print("ring")
			rcol = col
			rlwd = lwd
			rlty = lty
			
			if (!is.null(ringopt[["col"]])) rcol = ringopt[["col"]]
			if (!is.null(ringopt[["lwd"]])) rlwd = ringopt[["lwd"]]
			if (!is.null(ringopt[["lty"]])) rlty = ringopt[["lty"]]
			
			plot(SC@ringobj, xlim=xlim, ylim=ylim, col=rcol, lwd=rlwd, lty=rlty,
				 add=add, axes=axes, ..., setParUsrBB=setParUsrBB)
				
		} else if (i == 4 & !is.null(SC@polyobj)) { #plot polygons
			print("poly")
			pcol = col
			if (!is.null(polyopt[["col"]])) pcol = polyopt[["col"]]
			
			plot(SC@polyobj, col=pcol, border=border, add=add, xlim=xlim, ylim=ylim,
				 xpd=xpd, density=density, angle=angle, pbg=pbg, axes=axes, ...,
				 setParUsrBB=setParUsrBB)
		}
	}
}
		
setMethod("plot", signature(x = "SpatialCollections", y = "missing"),
	function(x, y, ...) plotSpatialCollections(x, ...))