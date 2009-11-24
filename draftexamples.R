system("R CMD SHLIB *.c -lgeos_c")
system("R CMD SHLIB *.c -lgeos_c")
dyn.load("rgeos.so")
.Call("rgeos_Init")
library(sp)
data(meuse)
coordinates(meuse) <- c("x", "y")
zz <- .Call("rgeos_inout", meuse)
.Call("rgeos_finish")
dyn.unload("rgeos.so")
system("R CMD SHLIB *.c -lgeos_c")
dyn.load("rgeos.so")
.Call("rgeos_Init")
zz <- .Call("rgeos_inout", meuse)
str(zz)
c(bbox(meuse))
all.equal(as(meuse, "SpatialPoints"), zz)
savehistory("tues091110.Rhistory")

library(sp)
p1 <- cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0))
p2 <- cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3))
p3 <- cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20))
Pls <- Polygons(list(Polygon(p1), Polygon(p2), Polygon(p3)), ID="1")

.Call("rgeos_what", Pls)

library(maptools)
Pls1 <- checkPolygonsHoles(Pls)


library(sp)
p1 <- Polygon(cbind(x=c(0, 0, 10, 10, 0), y=c(0, 10, 10, 0, 0)))
p2 <- Polygon(cbind(x=c(3, 3, 7, 7, 3), y=c(3, 7, 7, 3, 3)))
p8 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(1, 2, 2, 1, 1)))
p9 <- Polygon(cbind(x=c(1, 1, 2, 2, 1), y=c(5, 6, 6, 5, 5)))
p3 <- Polygon(cbind(x=c(20, 20, 30, 30, 20), y=c(20, 30, 30, 20, 20)))
p4 <- Polygon(cbind(x=c(21, 21, 29, 29, 21), y=c(21, 29, 29, 21, 21)))
p5 <- Polygon(cbind(x=c(22, 22, 28, 28, 22), y=c(22, 28, 28, 22, 22)))
p6 <- Polygon(cbind(x=c(23, 23, 27, 27, 23), y=c(23, 27, 27, 23, 23)))
p7 <- Polygon(cbind(x=c(13, 13, 17, 17, 13), y=c(13, 17, 17, 13, 13)))
p10 <- Polygon(cbind(x=c(24, 24, 26, 26, 24), y=c(24, 26, 26, 24, 24)))
lp <- list(p1, p2, p7, p6, p5, p4, p3, p8, p9, p10)
Pls <- Polygons(lp, ID="1")
#plot(SpatialPolygons(list(Pls)))

#system("R CMD SHLIB *.c -lgeos_c")
dyn.load("rgeos.so")
.Call("rgeos_Init")
source("../R/rgeos.R")
zz <- checkPolygonsGEOS(Pls)
zzz <- checkPolygonsGEOS(zz)



#lmat <- .Call("rgeos_PolygonsContain", Pls, NULL)


Er0 <- which(rowSums(lmat) == 0 & colSums(lmat) == 0)
Er1p <- which(rowSums(lmat) > 0 & colSums(lmat) == 0)
simple <- logical(length(Er1p))
for (i in 1:length(Er1p)) {
Iri <- which(lmat[Er1p[i],])
simple[i] <- all(colSums(lmat[,Iri]) == 1)
}


Iri <- which(lmat[Er1p[2],])
colSums(lmat[,Iri])



