
RGEOSArea = function(g1) {}

RGEOSLength = function(g1) {}

RGEOSDistance = function(g1, g2) {}

RGEOSisWithinDistance = function(g1,g2,dist) {
    return( RGEOSDistance(g1,g2) <= dist )
}

RGEOSHausdorffDistance = function(g1, g2) {}

RGEOSHausdorffDistanceDensify = function(g1, g2, densifyFrac) {}
