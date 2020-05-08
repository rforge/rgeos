\name{gMakeValid}
\alias{gMakeValid}

\title{Make invalid geometries valid}
\description{Function returns a valid geometry, not available before GEOS 3.8.0}
\usage{
gMakeValid(spgeom, byid=FALSE, id = NULL)
}

\arguments{
  \item{spgeom}{sp object as defined in package sp}
  \item{byid}{Logical determining if the function should be applied across subgeometries (TRUE) or the entire object (FALSE)}
  \item{id}{Character vector defining id labels for the resulting geometries, if unspecified returned geometries will be labeled based on their parent geometries' labels.}
}

\details{Returns a valid geometry or collection of geometries of different types}

\author{Roger Bivand}

\seealso{\link{SpatialCollections-class}}


\examples{
# Based on test geometries from sf
X <- readWKT("POLYGON ((0 0, 0.5 0, 0.5 0.5, 0.5 0, 1 0, 1 1, 0 1, 0 0))")
gIsValid(X)
class(X)
row.names(X)
Y <- gMakeValid(X)
gIsValid(Y)
class(Y)
plot(slot(Y, "polyobj"))
plot(slot(Y, "lineobj"), add=TRUE, col="red")
row.names(slot(Y, "polyobj"))
row.names(slot(Y, "lineobj"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

