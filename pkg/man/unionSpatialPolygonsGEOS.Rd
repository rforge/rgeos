\name{unionSpatialPolygonsGEOS}
\alias{unionSpatialPolygonsGEOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aggregate Polygons in a SpatialPolygons object}
\description{The function aggregates Polygons in a SpatialPolygons object, according to the IDs vector specifying which input Polygons belong to which output Polygons; internal boundaries are dissolved using the GEOS Unary Union operation.}
\usage{
unionSpatialPolygonsGEOS(SpP, IDs, threshold = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SpP}{A SpatialPolygons object as defined in package sp}
  \item{IDs}{A vector defining the output Polygons objects, equal in length to the length of the polygons slot of SpRs; it may be character, integer, or factor (try table(factor(IDs)) for a sanity check)}
  \item{threshold}{if not NULL, an area measure below which slivers will be discarded (some polygons have non-identical boundaries, for instance along rivers, generating slivers on union which are artefacts, not real sub-polygons)}
}

\value{Returns an aggregated SpatialPolygons object named with the aggregated IDs values in their sorting order; see the ID values of the output object to view the order.}

\section{Warning}{When using GEOS Unary Union, it has been found that some polygons are not dissolved correctly when the absolute values of the coordinates are very small. No work-around is available at present; see the example below for a case.}

\author{Roger Bivand}

\examples{
library(maptools)
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +datum=NAD27"))
slot(nc1, "polygons") <- lapply(slot(nc1, "polygons"), checkPolygonsGEOS)
lps <- coordinates(nc1)
ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)
reg4 <- RGEOSUnionCascaded(nc1, ID)
sapply(slot(reg4, "polygons"), function(i) slot(i, "ID"))
par(mfrow=c(2,1))
plot(nc1)
plot(reg4)
par(mfrow=c(1,1))
gt <- GridTopology(c(0.05,0.05), c(0.1,0.1), c(2,2))
set.seed(1)
xv <- rnorm(length(coordinates(gt)[,1]))
xvs <- ifelse(xv > 0.2,1,0)
grd <- SpatialGridDataFrame(gt, data.frame(xvs))
spix <- as(grd, "SpatialPixelsDataFrame")
spol <- as(spix, "SpatialPolygonsDataFrame")
slot(spol, "polygons") <- lapply(slot(spol, "polygons"), checkPolygonsGEOS)
image(grd, axes=TRUE)
spol1 <- RGEOSUnionCascaded(as(spol, "SpatialPolygons"), as.character(spol$xvs))
plot(spol1, add=TRUE)
gtx <- GridTopology(c(50,50), c(100,100), c(2,2))
grdx <- SpatialGridDataFrame(gtx, data.frame(xvs))
spixx <- as(grdx, "SpatialPixelsDataFrame")
spolx <- as(spixx, "SpatialPolygonsDataFrame")
slot(spolx, "polygons") <- lapply(slot(spolx, "polygons"), checkPolygonsGEOS)
image(grdx, axes=TRUE)
spol1x <- RGEOSUnionCascaded(as(spolx, "SpatialPolygons"),
  as.character(spol$xvs))
plot(spol1x, add=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

