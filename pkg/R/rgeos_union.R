unionSpatialPolygonsGEOS <- function(SpP, IDs, threshold=NULL) {
    if (!inherits(SpP, "SpatialPolygons")) stop("not a SpatialPolygons object")
    if (missing(IDs)) stop("IDs required")
    if (length(row.names(SpP)) != length(IDs)) stop("input lengths differ")
    tab <- table(factor(IDs))
    n <- length(tab)
    IDss <- as.character(names(tab))
    reg <- match(IDs, IDss)
    belongs <- lapply(1:n, function(x) as.integer(which(x == reg)))
    if (is.null(threshold)) threshold <- 0.0
    threshold <- as.double(threshold)
    res <- .Call("rgeos_SpatialPolygonsUnion", .RGEOS_HANDLE, SpP, belongs, IDss, threshold,
        PACKAGE="rgeos")
    res
}
