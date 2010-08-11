\name{checkHolesGPC}
\alias{checkHolesGPC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hole finding for pts slot of gpc.poly objects}
\description{To use the pts slot of gpc.poly objects in GEOS, the OGC Simple Features status (SFS) of the member polyons must be established, that is which rings are exterior rings, which are interior rings (holes) and which holes belong to which exterior rings. The function returns this both in the hole values of the polygon objects in the list, and as a comment encoding which hole (!= 0) belongs to which exterior ring (0). This may be used subsequently to control plotting, and for passing such enhanced objects to GEOS functions.}
\usage{
checkHolesGPC(gpclist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gpclist}{pts slot of gpc.poly object}
}

\author{Roger Bivand}



\examples{
p1 <- list(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0), hole=FALSE) # I
p2 <- list(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3), hole=FALSE) # H
p8 <- list(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1), hole=FALSE) # H
p8a <- list(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1), hole=FALSE) # I (full)
p9 <- list(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5), hole=FALSE) # H
p3 <- list(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20), hole=FALSE) # I
p4 <- list(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21), hole=FALSE) # H
p5 <- list(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22), hole=FALSE) # I
p6 <- list(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23), hole=FALSE) # H
p7 <- list(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13), hole=FALSE) # I
p10 <- list(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24), hole=FALSE) # I
p11 <- list(x=c(24.25, 24.25, 25.75, 25.75, 24.25), y=c(24.25, 25.75, 25.75, 24.25, 24.25), hole=FALSE) # H
p12 <- list(x=c(24.5, 24.5, 25.5, 25.5, 24.5), y=c(24.5, 25.5, 25.5, 24.5, 24.5), hole=FALSE) # I
p13 <- list(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75), hole=FALSE) # H
p13a <- list(x=c(24.75, 24.75, 25.25, 25.25, 24.75), y=c(24.75, 25.25, 25.25, 24.75, 24.75), hole=FALSE) # I (full)
lp <- list(p1, p2, p13, p7, p6, p5, p4, p3, p8, p11, p12, p9, p10, p8a, p13a)
#           1   2    3   4   5   6   7   8   9   10   11  12   13   14    15
#           0   1   11   0   6   0   8   0   1   13    0   1    0    0     0
#           I   H    H   I   H   I   H   I   H    H    I   H    I    I     I
comment(lp)
zz <- checkHolesGPC(lp)
comment(zz)
sapply(zz, "[[", "hole")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spatial}

