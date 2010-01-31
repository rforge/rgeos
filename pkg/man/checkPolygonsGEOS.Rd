\name{checkPolygonsGEOS}
\alias{checkPolygonsGEOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check holes in Polygons objects}
\description{
  The function checks holes in Polygons objects, using the GEOS Contains operation. The function will set single polygon contours to hole=FALSE, and if multiple polygon contours are holes, will set them TRUE. It sets a comment attribute on the Polygons object specifying which Polygon exterior ring contains which hole - Polygon interior ring.
}
\usage{
checkPolygonsGEOS(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An Polygons object as defined in package sp}
}
\details{The comment attached to the Polygons object is an afterthought, which may be added to a future revision of the class. Values of zero encode exterior rings, while non-zero values are interior rings (holes), taking the value of the exterior ring to which they belong.}

\value{An Polygons object re-created from the input object.}

\author{Roger Bivand}

\examples{
library(maptools)
nc1 <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1],
 proj4string=CRS("+proj=longlat +ellps=clrk66"))
pl <- slot(nc1, "polygons")
sapply(slot(pl[[4]], "Polygons"), function(x) slot(x, "hole"))
pl[[4]] <- Polygons(list(slot(pl[[4]], "Polygons")[[1]],
 Polygon(slot(slot(pl[[4]], "Polygons")[[2]], "coords"), hole=TRUE),
 slot(pl[[4]], "Polygons")[[3]]), slot(pl[[4]], "ID"))
comment(pl[[4]])
sapply(slot(pl[[4]], "Polygons"), function(x) slot(x, "hole"))
pl_new <- lapply(pl, checkPolygonsGEOS)
sapply(slot(pl_new[[4]], "Polygons"), function(x) slot(x, "hole"))
comment(pl_new[[4]])
srs <- slot(slot(pl[[1]], "Polygons")[[1]], "coords")
hle2 <- structure(c(-81.64093, -81.38380, -81.34165, -81.66833, -81.64093,
 36.57865, 36.57234, 36.47603, 36.47894, 36.57865), 
 .Dim = as.integer(c(5, 2)))
hle3 <- structure(c(-81.47759, -81.39118, -81.38486, -81.46705, -81.47759, 
 36.56289, 36.55659, 36.49907, 36.50380, 36.56289),
 .Dim = as.integer(c(5, 2)))
x <- Polygons(list(Polygon(srs), Polygon(hle2), Polygon(hle3)),
 ID=slot(pl[[1]], "ID"))
sapply(slot(x, "Polygons"), function(x) slot(x, "hole"))
res <- checkPolygonsGEOS(x)
sapply(slot(res, "Polygons"), function(x) slot(x, "hole"))
comment(res)
opar <- par(mfrow=c(1,2))
SPx <- SpatialPolygons(list(x))
plot(SPx, col="magenta", pbg="cyan")
text(t(sapply(slot(x, "Polygons"), slot, "labpt")),
 labels=sapply(slot(x, "Polygons"), function(i) slot(i, "hole")), cex=0.6)
title(xlab="Hole slot values before checking")
SPres <- SpatialPolygons(list(res))
plot(SPres, col="magenta", pbg="cyan")
text(t(sapply(slot(res, "Polygons"), slot, "labpt")),
 labels=sapply(slot(res, "Polygons"), function(i) slot(i, "hole")), cex=0.6)
title(xlab="Hole slot values after checking")
par(opar)
p1 <- Polygon(cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0))) # I
p2 <- Polygon(cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3))) # H
p8 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1))) # H
p8a <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1))) # I (full)
p9 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5))) # H
p3 <- Polygon(cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20))) # I
p4 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21))) # H
p5 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22))) # I
p6 <- Polygon(cbind(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23))) # H
p7 <- Polygon(cbind(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13))) # I
p10 <- Polygon(cbind(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24))) # I
p11 <- Polygon(cbind(x=c(24.25, 24.25, 25.75, 25.75, 24.25), y=c(24.25, 25.75, 25.75, 24.25, 24.25))) # H
p12 <- Polygon(cbind(x=c(24.5, 24.5, 25.5, 25.5, 24.5), y=c(24.5, 25.5, 25.5, 24.5, 24.5))) # I
p13 <- Polygon(cbind(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75))) # H
p13a <- Polygon(cbind(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75))) # I (full)
lp <- list(p1, p2, p13, p7, p6, p5, p4, p3, p8, p11, p12, p9, p10, p8a, p13a)
#           1   2    3   4   5   6   7   8   9   10   11  12   13   14    15
#           0   1   11   0   6   0   8   0   1   13    0   1    0    0     0
#           I   H    H   I   H   I   H   I   H    H    I   H    I    I     I
Pls <- Polygons(lp, ID="1")
comment(Pls)
zz <- checkPolygonsGEOS(Pls)
comment(zz)
slot(zz, "plotOrder")
sapply(slot(zz, "Polygons"), slot, "hole")
sapply(slot(zz, "Polygons"), slot, "area")
opar <- par(mfrow=c(1,2))
plot(SpatialPolygons(list(Pls)), col="magenta", pbg="cyan")
title(xlab="Hole slot values before checking")
plot(SpatialPolygons(list(zz)), col="magenta", pbg="cyan")
title(xlab="Hole slot values after checking")
par(opar)
data(gpcholes)
opar <- par(mfrow=c(2,2))
plot(SpatialPolygons(list(h1pl)), col="red", pbg="white", border="blue")
plot(SpatialPolygons(list(checkPolygonsGEOS(h1pl))), col="red", pbg="white", border="blue")
plot(SpatialPolygons(list(h2pl)), col="red", pbg="white", border="blue")
plot(SpatialPolygons(list(checkPolygonsGEOS(h2pl))), col="red", pbg="white", border="blue")
par(opar)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

