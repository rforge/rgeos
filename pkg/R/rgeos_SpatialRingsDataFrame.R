setClass("SpatialRingsDataFrame",
    representation("SpatialRings", data = "data.frame"),
    validity = function(object) {
        if (!inherits(object@data, "data.frame"))
            stop("data should be of class data.frame")
        if (nrow(object@data) != length(object@rings))
            stop("number of rows in data.frame and SpatialRings don't match")
        return(TRUE)
    }
)

SpatialRingsDataFrame = function(sr, data, match.ID = TRUE) {
    if (match.ID) {
        sr_IDs <- row.names(sr)
        data_IDs <- row.names(data)
        mtch <- match(sr_IDs, data_IDs)
        if (any(is.na(mtch)))
            stop("row.names of data and Rings IDs do not match")
        if (length(unique(mtch)) != length(sr_IDs))
            stop("row.names of data and Rings IDs do not match")
        data <- data[mtch, , drop=FALSE]
    }
    if (nrow(data) != length(sr@rings))
        stop("length of data.frame does not match number of Ring elements")
    
    return( new("SpatialRingsDataFrame", sr, data = data) )
}

setAs("SpatialRingsDataFrame","SpatialRings", function(from) SpatialRings(from@rings))
setAs("SpatialRingsDataFrame", "data.frame", function(from) from@data)

setMethod("names","SpatialRingsDataFrame", function(x) names(x@data))    
setReplaceMethod("names", signature(x = "SpatialRingsDataFrame", value = "character"),
                 function(x, value) { names(x@data)<-value; x })

setMethod("row.names","SpatialRingsDataFrame", function(x) sapply(slot(x, "rings"), slot, "ID"))    
setReplaceMethod("row.names", signature(x = "SpatialRingsDataFrame", value = "character"),
              function(x, value) spChFIDs(x, value))


setMethod("[", c("SpatialRingsDataFrame", "ANY", "ANY"), 
    function(x, i, j, ... , drop = TRUE) {
        missing.i = missing(i)
        missing.j = missing(j)
        nargs = nargs() # e.g., a[3,] gives 2 for nargs, a[3] gives 1.
        
        if (missing.i && missing.j) {
            i = TRUE
            j = TRUE
        } else if (missing.j && !missing.i) {
            if (nargs == 2) {
                j = i
                i = TRUE
            } else {
                j = TRUE
            }
        } else if (missing.i && !missing.j) {
            i = TRUE
        }
        
        if (is.matrix(i)) stop("matrix argument not supported in SpatialRingsDataFrame selection")
        if (is.logical(i)) {
            if (length(i) == 1 && i)
                i = 1:length(x@rings)
            else
                i = which(i)
        } else if (is.character(i)) {
                i = match(i, row.names(x))
        }
        
        if (any(is.na(i))) stop("NAs not permitted in row index")
        
        x@rings = x@rings[i]
        x@data = x@data[i, j, ..., drop = FALSE]
        x@bbox = .bboxSRs(x@rings)
        
        return(x)
    }
)

#setMethod("lines","SpatialRingsDataFrame", function(x, y = NULL, ...) lines(as(x, "SpatialRings"), ...))    
setMethod("dim","SpatialRingsDataFrame", function(x) dim(x@data))


chFIDsSpatialRingsDataFrame <- function(obj, x) {
    SR <- as(obj, "SpatialRings")
    SRx <- spChFIDs(SR, x)
    df <- as(obj, "data.frame")
    row.names(df) <- sapply(slot(SRx, "rings"), function(x) slot(x, "ID"))
    SpatialRingsDataFrame(SRx, data=df)
}

setMethod("spChFIDs", signature(obj="SpatialRingsDataFrame", x="character"), chFIDsSpatialRingsDataFrame)
