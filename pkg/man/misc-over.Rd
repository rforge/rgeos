\name{over}
\alias{overGeomGeom}
\alias{overGeomGeomDF}

\title{Find spatial join or intersections}
\description{Find spatial join or intersections}
\usage{
overGeomGeom(x, y, returnList = FALSE, fn = NULL, ...)
overGeomGeomDF(x, y, returnList = FALSE, fn = NULL, ...)
}

\arguments{
  \item{x}{see \link[sp]{over}}
  \item{y}{see \link[sp]{over}}
  \item{returnList}{see \link[sp]{over}}
  \item{fn}{see \link[sp]{over}}
  \item{...}{see \link[sp]{over}}
}

\value{see \link[sp]{over}}

\author{Edzer Pebesma}
\examples{
p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")

overGeomGeom(p1,p2)
}
\keyword{spatial}
