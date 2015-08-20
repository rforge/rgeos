order_relations = function(rel, minDimension = 0) {
	rel = sapply(rel, function(x)
			paste0(substring(x, c(1,4),c(2,5)), collapse=""))
		# our interest is in chars
		# 1-2 = inner of x with inner/border of y
		# 4-5 = border of x with inner/border of y
	ret = vector("numeric", length(rel)) * NA
	if (minDimension <= 0)
		ret[grep("0", rel, fixed = TRUE, useBytes = TRUE)] = 0
	if (minDimension <= 1) 
		ret[grep("1", rel, fixed = TRUE, useBytes = TRUE)] = 1
	if (minDimension <= 2)
		ret[grep("2", rel, fixed = TRUE, useBytes = TRUE)] = 2
	order(ret, decreasing = TRUE, na.last = NA)
}

listifyMatrix = function(x) { # put columns in list elements
	if (!is.list(x)) {
		if (!is.matrix(x)) # vector!
			x = matrix(x, 1, length(x))
		x = lapply(1:ncol(x), function(i) x[,i])
	}
	x
}

overGeomGeom = function(x, y, returnList = FALSE, fn = NULL, ...,
		minDimension = 0) {
	stopifnot(identicalCRS(x, y))
	if (gridded(x))
		x = as(x, "SpatialPolygons")
	if (gridded(y))
		y = as(y, "SpatialPolygons")

	ret = apply(gRelate(x, y, byid = TRUE), 2, 
		order_relations, minDimension = minDimension)
	ret = listifyMatrix(ret) # if not already list, create one now
	if (!returnList) # pick first or NA if length is 0:
		sapply(ret, function(x) (x)[1])
	else 
		ret
}

# taken from: overDFGeneric in sp; 
# if modified here, consider modifying there as well!
overGeomGeomDF = function(x, y, returnList = FALSE, fn = NULL, ...) {
    r = overGeomGeom(x, y, returnList = TRUE)
    #ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
	#  length(x) differs from length(r) in case of SpatialMultiPoints!!!
	#  reason to change is sp::overMultiPoints
    ret = overDF_for_rgeos(r, y@data, length(r), returnList, fn, ...)
    if (!returnList)
        row.names(ret) = row.names(r)
    ret
}

#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPolygons"),
#	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPoints", y = "SpatialLines"),
	        overGeomGeom)
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPoints"),
#	        overGeomGeom)
#setMethod("over",
#    signature(x = "SpatialPolygons", y = "SpatialPoints"),
#	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialLines"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialPolygons"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPoints"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPolygons"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialLines"),
	        overGeomGeom)

# all with DataFrame:
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPolygonsDataFrame"),
#	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPoints", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
#setMethod("over",
#    signature(x = "SpatialPoints", y = "SpatialPointsDataFrame"),
#	        overGeomGeomDF)
#setMethod("over",
#    signature(x = "SpatialPolygons", y = "SpatialPointsDataFrame"),
#	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPolygons", y = "SpatialPolygonsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPointsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPolygonsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)

# lines & grids:
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPixels"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialGrid"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialPixelsDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialLines", y = "SpatialGridDataFrame"),
	        overGeomGeomDF)
setMethod("over",
    signature(x = "SpatialPixels", y = "SpatialLines"),
	        overGeomGeom)
setMethod("over",
    signature(x = "SpatialGrid", y = "SpatialLinesDataFrame"),
	        overGeomGeomDF)
