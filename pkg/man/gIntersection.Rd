\name{gIntersection}
\alias{gIntersection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Geometry intersections}
\description{The function finds intersection points between sp objects}
\usage{
gIntersection(spgeom1, spgeom2, byid=FALSE, id=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spgeom1, spgeom2}{sp objects as defined in package sp}
  \item{byid}{Logical vector determining if the function should be applied across ids (TRUE) or the entire object (FALSE) for spgeom1 and spgeom2}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{The \code{gIntersection} function returns all intersections between the two input sp objects. FIXME The points found correspond to those returned by the spatstat \code{crossing.psp} function, but are annotated with Lines IDs.
}

\value{}

\author{Roger Bivand & Colin Rundel}

\examples{
library(maptools)
assign("rgeos", TRUE, envir=maptools:::.MAPTOOLS_CACHE)
xx <- readShapeSpatial(system.file("shapes/fylk-val-ll.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +datum=WGS84"))
bbxx <- bbox(xx)
wdb_lines <- system.file("share/wdb_borders_c.b", package="maptools")
xxx <- Rgshhs(wdb_lines, xlim=bbxx[1,], ylim=bbxx[2,])$SP
res <-gIntersection(xx, xxx)
plot(xx, axes=TRUE)
plot(xxx, lty=2, add=TRUE)
plot(res, add=TRUE, pch=16,col='red')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

