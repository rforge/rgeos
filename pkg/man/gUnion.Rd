\name{gUnion}
\alias{gUnion}
\alias{gUnionCascaded}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometry Union in an sp object}
\description{These functions aggregates geometries through the GEOS union operator.}
\usage{
gUnion(spgeom1, spgeom2, byid=FALSE, id=NULL)
gUnionCascaded(spgeom, id = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1,spgeom2}{sp objects (usually of class SpatialPolygon)}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
  \item{spgeom}{sp object with multiple subgeometries}
}

\value{Returns an aggregated sp object with geometries named with the given id values.}

\author{Roger Bivand & Colin Rundel}

\examples{
library(maptools)
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=NAD27"))
lps <- coordinates(nc1)
ID <- cut(lps[,1], quantile(lps[,1]), include.lowest=TRUE)
reg4 <- gUnionCascaded(nc1, ID)
row.names(reg4)
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
image(grd, axes=TRUE)
spol1 <- gUnionCascaded(spol, as.character(spol$xvs))
plot(spol1, add=TRUE)

gtx <- GridTopology(c(50,50), c(100,100), c(2,2))
grdx <- SpatialGridDataFrame(gtx, data.frame(xvs))
spixx <- as(grdx, "SpatialPixelsDataFrame")
spolx <- as(spixx, "SpatialPolygonsDataFrame")
image(grdx, axes=TRUE)
spol1x <- gUnionCascaded(spolx,as.character(spol$xvs))
plot(spol1x, add=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial,union}

