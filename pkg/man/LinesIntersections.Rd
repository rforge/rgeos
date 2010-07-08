\name{LinesIntersections}
\alias{LinesIntersections}
\alias{SpatialLinesIntersections}
\alias{PolygonsIntersections}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lines intersections}
\description{The function finds intersection points between Lines objects in Spatial Lines objects}
\usage{
SpatialLinesIntersections(SL1, SL2)
LinesIntersections(Lobj1, Lobj2)
PolygonsIntersections(Pobj1, Pobj2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SL1, SL2}{SpatialLines objects as defined in package sp}
  \item{Lobj1, Lobj2}{Lines objects as defined in package sp}
  \item{Pobj1, Pobj2}{Polygons objects as defined in package sp}
}
\details{The \code{SpatialLinesIntersections} function steps through all possible intersections between the Lines objects in the two input objects. If they intersect, the intersection point(s) are found and returned. The points found correspond to those returned by the spatstat \code{crossing.psp} function, but are annotated with Lines IDs.

}

\value{The \code{SpatialLinesIntersections} function returns a SpatialPointsDataFrame with id1 and id2 variables reporting which Lines objects intersect at each point, or NULL. The \code{LinesIntersections} function returns a matrix of coordinates or NULL, and the \code{PolygonsIntersections} function returns a Polygons object or NULL.}

\author{Roger Bivand}

\examples{
require(maptools)
if (require(rgeos)) {
assign("rgeos", TRUE, envir=maptools:::.MAPTOOLS_CACHE)
xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp",
 package="maptools")[1], proj4string=CRS("+proj=longlat +datum=WGS84"))
bbxx <- bbox(xx)
wdb_lines <- system.file("share/wdb_borders_c.b", package="maptools")
xxx <- Rgshhs(wdb_lines, xlim=bbxx[1,], ylim=bbxx[2,])$SP
#FIXME
#res <- SpatialLinesIntersections(xx, xxx)
plot(xx, axes=TRUE)
plot(xxx, lty=2, add=TRUE)
#plot(res, add=TRUE, pch=16)
print(row.names(xx))
print(row.names(xxx))
#print(summary(res))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

