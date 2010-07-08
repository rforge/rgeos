\name{thinnedSpatialPolyGEOS}
\alias{thinnedSpatialPolyGEOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Douglas-Peuker line generalization for Spatial Polygons}
\description{
The function applies the implementation of the Douglas-Peuker algorithm for line generalization or simplification to objects inheriting from Spatial Polygons. It preserves topology in the Simple Features sense, so is suitable for visualisation, but not for the subsequent analysis of the polygon boundaries, as artefacts may be created, and boundaries of neighbouring entities may be generalized differently. There are however the same number of Polygons objects, and those that touch also touch, but not necessarily along boundaries - often only at nodes where three polygons touch.
}
\usage{
thinnedSpatialPolyGEOS(SP, tolerance, minarea = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SP}{an object inheriting from the SpatialPolygons class}
  \item{tolerance}{the tolerance value in the metric of the input object}
  \item{minarea}{the smallest area of Polygon objects to be retained}
}

\value{An object of the same class as the input object}
\references{Douglas, D. and Peucker, T. (1973). Algorithms for 
the reduction of the number of points required to 
represent a digitized line or its caricature. 
\emph{The Canadian Cartographer} 10(2). 112-122.}
\author{Roger Bivand}


\examples{
library(maptools)
xx <- readShapeSpatial(system.file("shapes/sids.shp", package="maptools")[1],
      IDvar="FIPSNO", proj4string=CRS("+proj=longlat +ellps=clrk66"))
slot(xx, "polygons") <- lapply(slot(xx, "polygons"), checkPolygonsGEOS)
object.size(as(xx, "SpatialPolygons"))
#FIXME
#xxx <- thinnedSpatialPolyGEOS(xx, tolerance=0.05, minarea=0.001)
#object.size(as(xxx, "SpatialPolygons"))
#par(mfrow=c(2,1))
#plot(xx)
#plot(xxx)
#par(mfrow=c(1,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

