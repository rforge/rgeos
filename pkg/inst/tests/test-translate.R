library(testthat)
library(rgeos)

context("Translation functions")

test_that("translate points", {
    
    p = readWKT("POINT( 1 1 )")
    mp = readWKT("MULTIPOINT(1 1, 2 2, 3 3, 4 4, 5 5)") 
    gcp1 = readWKT("GEOMETRYCOLLECTION( POINT(1 1), POINT(2 2), POINT(3 3), POINT(4 4), POINT(5 5))")
    gcp2 = readWKT("GEOMETRYCOLLECTION( POINT(1 1), POINT(2 2), MULTIPOINT(3 3, 4 4, 5 5))")
    gcp3 = readWKT("GEOMETRYCOLLECTION( POINT(1 1), POINT(2 2), MULTIPOINT(3 3, 4 4),POINT(5 5))")
    gcp4 = readWKT("GEOMETRYCOLLECTION( MULTIPOINT(1 1, 2 2), MULTIPOINT(3 3, 4 4, 5 5))")
    gcp5 = readWKT("GEOMETRYCOLLECTION( MULTIPOINT(1 1, 2 2), MULTIPOINT(3 3, 4 4),POINT(5 5))")
    
    spp = SpatialPoints(list(x=1,y=1))
    spmp = SpatialPoints(list(x=1:5,y=1:5))
    
    rownames(spp@coords) = c("1")
    expect_that( p, is_identical_to(spp) )
    expect_that( spp , is_identical_to( doubletranslate(spp)))
    
    rownames(spmp@coords) = c("1","1","1","1","1")
    expect_that( mp, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))
    
    rownames(spmp@coords) = c("1","2","3","4","5")
    expect_that( gcp1, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))
    
    rownames(spmp@coords) = c("1","2","3","3","3")
    expect_that( gcp2, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))

    rownames(spmp@coords) = c("1","2","3","3","4")
    expect_that( gcp3, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))
    
    rownames(spmp@coords) = c("1","1","2","2","2")    
    expect_that( gcp4, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))
    
    rownames(spmp@coords) = c("1","1","2","2","3")
    expect_that( gcp5, is_identical_to(spmp) )
    expect_that( spmp, is_identical_to( doubletranslate(spmp)))

    
    expect_that( p   , is_identical_to( doubletranslate(p) ))
    expect_that( mp  , is_identical_to( doubletranslate(mp) ))
    expect_that( gcp1, is_identical_to( doubletranslate(gcp1) ))
    expect_that( gcp2, is_identical_to( doubletranslate(gcp2) ))
    expect_that( gcp3, is_identical_to( doubletranslate(gcp3) ))
    expect_that( gcp4, is_identical_to( doubletranslate(gcp4) ))
    expect_that( gcp5, is_identical_to( doubletranslate(gcp5) ))
    
})


test_that("translate lines", {

    l = readWKT("LINESTRING (1 1, 2 2, 3 3, 4 4)")

    ml1 = readWKT("MULTILINESTRING ((1 1, 2 2, 3 3, 4 4),(1 1, 2 2, 3 3, 4 4))")
    ml2 = readWKT("MULTILINESTRING ((1 1, 2 2, 3 3, 4 4),(4 1, 3 2, 2 3, 1 4))")

    gcl1 = readWKT("GEOMETRYCOLLECTION( LINESTRING (1 1, 2 2, 3 3, 4 4), LINESTRING (1 1, 2 2, 3 3, 4 4) )")
    gcl2 = readWKT("GEOMETRYCOLLECTION( LINESTRING (1 1, 2 2, 3 3, 4 4), MULTILINESTRING ((1 1, 2 2, 3 3, 4 4),(4 1, 3 2, 2 3, 1 4)), LINESTRING (1 1, 2 2, 3 3, 4 4) )")


    Line1 = Line(cbind( x=1:4,y=1:4 ))
    Line2 = Line(cbind( x=4:1,y=1:4 ))
    
    Linesl = Lines( list(Line1), ID = "1" )
    Linesl2 = Lines( list(Line1), ID = "2" )
    
    Linesml1 = Lines( list(Line1, Line1), ID = "1" )
    Linesml2 = Lines( list(Line1, Line2), ID = "1" )
    
    #FIXME - weirdness with rownames in the bbox
    spl    = SpatialLines( list(Linesl) ); rownames(spl@bbox) = c("x","y")
    spml1  = SpatialLines( list(Linesml1) ); rownames(spml1@bbox) = c("x","y")
    spml2  = SpatialLines( list(Linesml2) ); rownames(spml2@bbox) = c("x","y")
    
    spgcl1 = SpatialLines( list(Linesl,Linesl2) ); rownames(spgcl1@bbox) = c("x","y")
    Linesml2@ID = "2"
    Linesl2@ID = "3"
    spgcl2 = SpatialLines( list(Linesl,Linesml2,Linesl2) ); rownames(spgcl2@bbox) = c("x","y")


    expect_that( l   , is_identical_to(spl) )
    expect_that( ml1 , is_identical_to(spml1) )
    expect_that( ml2 , is_identical_to(spml2) )
    expect_that( gcl1, is_identical_to(spgcl1) )
    expect_that( gcl2, is_identical_to(spgcl2) )
    
    expect_that( spl   , is_identical_to( doubletranslate(spl)))
    expect_that( spml1 , is_identical_to( doubletranslate(spml1)))
    expect_that( spml2 , is_identical_to( doubletranslate(spml2)))
    expect_that( spgcl1, is_identical_to( doubletranslate(spgcl1)))
    expect_that( spgcl2, is_identical_to( doubletranslate(spgcl2)))
    
    expect_that( l   , is_identical_to( doubletranslate(l) ))
    expect_that( ml1 , is_identical_to( doubletranslate(ml1) ))
    expect_that( ml2 , is_identical_to( doubletranslate(ml2) ))
    expect_that( gcl1, is_identical_to( doubletranslate(gcl1) ))
    expect_that( gcl2, is_identical_to( doubletranslate(gcl2) ))

})   


test_that("translate linear ring", {

    lr1 = readWKT("LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1)")
    lr2 = readWKT("LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1)")
    gclr1 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1), LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1) )")
    gclr2 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1), LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1) )")
    gclr3 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1), LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1) )")
    
    Line1 = Line(cbind( x=c(1,1,2,2,1),y=c(1,2,2,1,1) ))
    Line2 = Line(cbind( x=c(1,2,2,1,1),y=c(1,1,2,2,1) ))
    
    Lineslr11 = Lines( list(Line1), ID="1" )
    Lineslr12 = Lines( list(Line1), ID="2" )
    
    Lineslr21 = Lines( list(Line2), ID="1" ) 
    Lineslr22 = Lines( list(Line2), ID="2" ) 
    
    
    splr1   = SpatialLines( list(Lineslr11) ); rownames(splr1@bbox) = c("x","y")
    splr2   = SpatialLines( list(Lineslr21) ); rownames(splr2@bbox) = c("x","y")
    spgclr1 = SpatialLines( list(Lineslr11,Lineslr12) ); rownames(spgclr1@bbox) = c("x","y")
    spgclr2 = SpatialLines( list(Lineslr11,Lineslr22) ); rownames(spgclr2@bbox) = c("x","y")
    spgclr3 = SpatialLines( list(Lineslr21,Lineslr22) ); rownames(spgclr3@bbox) = c("x","y")
    

    expect_that( lr1  , is_identical_to(splr1) )
    expect_that( lr2  , is_identical_to(splr2) )
    expect_that( gclr1, is_identical_to(spgclr1) )
    expect_that( gclr2, is_identical_to(spgclr2) )
    expect_that( gclr3, is_identical_to(spgclr3) )
    
    expect_that( splr1  , is_identical_to( doubletranslate(splr1)))
    expect_that( splr2  , is_identical_to( doubletranslate(splr2)))
    expect_that( spgclr1, is_identical_to( doubletranslate(spgclr1)))
    expect_that( spgclr2, is_identical_to( doubletranslate(spgclr2)))
    expect_that( spgclr3, is_identical_to( doubletranslate(spgclr3)))
    
    expect_that( lr1  , is_identical_to( doubletranslate(lr1) ))
    expect_that( lr2  , is_identical_to( doubletranslate(lr2) ))
    expect_that( gclr1, is_identical_to( doubletranslate(gclr1) ))
    expect_that( gclr2, is_identical_to( doubletranslate(gclr2) ))
    expect_that( gclr3, is_identical_to( doubletranslate(gclr3) ))

})


test_that("translate simple polygon", {

    p=readWKT("POLYGON((1 1,5 1,5 5,1 5,1 1))")
    ph1=readWKT("POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2))")
    ph2=readWKT("POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2), (3 3,3 4,4 4,4 3,3 3) ) ")
    
    mp=readWKT("MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1)),((3 5,5 7, 1 7, 3 5)))")
    mph1=readWKT("MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((3 5,5 7, 1 7, 3 5)))")
    mph2=readWKT("MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1)),((3 5,5 7, 1 7, 3 5),(3 5.5,4 6.5, 2 6.5, 3 5.5) ))")
    mph3=readWKT("MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)),((3 5,5 7, 1 7, 3 5),(3 5.5,4 6.5, 2 6.5, 3 5.5)) )")
    
    Poly1 = Polygon(list(x=c(1,5,5,1,1),y=c(1,1,5,5,1)), hole=FALSE)
    Poly2 = Polygon(list(x=c(2,2,3,3,2),y=c(2,3,3,2,2)), hole=TRUE)
    Poly3 = Polygon(list(x=c(3,3,4,4,3),y=c(3,4,4,3,3)), hole=TRUE)
    Poly4 = Polygon(list(x=c(3,5,1,3),y=c(5,7,7,5)), hole=FALSE)
    Poly5 = Polygon(list(x=c(3,4,2,3),y=c(5.5,6.5,6.5,5.5)), hole=TRUE)
    Poly6 = Polygon(list(x=c(5,7,7,5),y=c(3,5,1,3)), hole=FALSE)
    
    Polysp = Polygons(list(Poly1), ID="1")
    Polysph1 = Polygons(list(Poly1,Poly2), ID="1")
    Polysph2 = Polygons(list(Poly1,Poly2,Poly3), ID="1")
    
    Polysmp = Polygons(list(Poly1,Poly4), ID="1" )
    Polysmph1 = Polygons(list(Poly1,Poly2,Poly4), ID="1" )
    Polysmph2 = Polygons(list(Poly1,Poly4,Poly5), ID="1" )
    Polysmph3 = Polygons(list(Poly1,Poly2,Poly4,Poly5), ID="1" )
    
    spp     = SpatialPolygons( list(Polysp) )
    spph1   = SpatialPolygons( list(Polysph1) )
    spph2   = SpatialPolygons( list(Polysph2) )
    spmp    = SpatialPolygons( list(Polysmp) )
    spmph1  = SpatialPolygons( list(Polysmph1) )
    spmph2  = SpatialPolygons( list(Polysmph2) )
    spmph3  = SpatialPolygons( list(Polysmph3) )
    
    attr(spp@polygons[[1]],"comment") = "0"
    attr(spph1@polygons[[1]],"comment") = "0 1"
    attr(spph2@polygons[[1]],"comment") = "0 1 1"
    attr(spmp@polygons[[1]],"comment") = "0 0"
    attr(spmph1@polygons[[1]],"comment") = "0 1 0"
    attr(spmph2@polygons[[1]],"comment") = "0 0 2"
    attr(spmph3@polygons[[1]],"comment") = "0 1 0 3"
    
    expect_that( p    , is_identical_to(spp) )
    expect_that( ph1  , is_identical_to(spph1) )
    expect_that( ph2  , is_identical_to(spph2) )
    expect_that( mp   , is_identical_to(spmp) )
    expect_that( mph1 , is_identical_to(spmph1) )
    expect_that( mph2 , is_identical_to(spmph2) )
    expect_that( mph3 , is_identical_to(spmph3) )
    
    expect_that( spp   , is_identical_to( doubletranslate(spp)))
    expect_that( spph1 , is_identical_to( doubletranslate(spph1)))
    expect_that( spph2 , is_identical_to( doubletranslate(spph2)))
    expect_that( spmp  , is_identical_to( doubletranslate(spmp)))
    expect_that( spmph1, is_identical_to( doubletranslate(spmph1)))
    expect_that( spmph2, is_identical_to( doubletranslate(spmph2)))
    expect_that( spmph3, is_identical_to( doubletranslate(spmph3)))
    
    expect_that( p   , is_identical_to( doubletranslate(p)))
    expect_that( ph1 , is_identical_to( doubletranslate(ph1)))
    expect_that( ph2 , is_identical_to( doubletranslate(ph2)))
    expect_that( mp  , is_identical_to( doubletranslate(mp)))
    expect_that( mph1, is_identical_to( doubletranslate(mph1)))
    expect_that( mph2, is_identical_to( doubletranslate(mph2)))
    expect_that( mph3, is_identical_to( doubletranslate(mph3)))
    
})


test_that("translate polygon collection", {

    gcp1=readWKT("GEOMETRYCOLLECTION( POLYGON((1 1,5 1,5 5,1 5,1 1)), POLYGON((3 5,5 7, 1 7, 3 5)), POLYGON((5 3,7 5,7 1,5 3)) )")
    gcp2=readWKT("GEOMETRYCOLLECTION( MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1)), ((3 5,5 7, 1 7, 3 5)) ), POLYGON((5 3,7 5,7 1,5 3)) )")
    gcp3=readWKT("GEOMETRYCOLLECTION( POLYGON((1 1,5 1,5 5,1 5,1 1)), MULTIPOLYGON( ((3 5,5 7, 1 7, 3 5)), ((5 3,7 5,7 1,5 3)) ))")
    gcp4=readWKT("GEOMETRYCOLLECTION( MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1)), ((3 5,5 7, 1 7, 3 5)), ((5 3,7 5,7 1,5 3)) ))")
    
    gcph1=readWKT("GEOMETRYCOLLECTION( POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)), POLYGON((3 5,5 7, 1 7, 3 5),(3 5.5,4 6.5, 2 6.5, 3 5.5)), POLYGON((5 3,7 5,7 1,5 3)) )")
    gcph2=readWKT("GEOMETRYCOLLECTION( MULTIPOLYGON( ((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)), ((3 5,5 7, 1 7, 3 5),(3 5.5,4 6.5, 2 6.5, 3 5.5)) ), POLYGON((5 3,7 5,7 1,5 3)) )")
    gcph3=readWKT("GEOMETRYCOLLECTION( POLYGON((1 1,5 1,5 5,1 5,1 1),(2 2,2 3,3 3,3 2,2 2)), MULTIPOLYGON( ((3 5,5 7, 1 7, 3 5),(3 5.5,4 6.5, 2 6.5, 3 5.5)), ((5 3,7 5,7 1,5 3)) ))")
    
    
    Poly1 = Polygon(list(x=c(1,5,5,1,1),y=c(1,1,5,5,1)), hole=FALSE)
    Poly2 = Polygon(list(x=c(2,2,3,3,2),y=c(2,3,3,2,2)), hole=TRUE)
    Poly3 = Polygon(list(x=c(3,3,4,4,3),y=c(3,4,4,3,3)), hole=TRUE)
    Poly4 = Polygon(list(x=c(3,5,1,3),y=c(5,7,7,5)), hole=FALSE)
    Poly5 = Polygon(list(x=c(3,4,2,3),y=c(5.5,6.5,6.5,5.5)), hole=TRUE)
    Poly6 = Polygon(list(x=c(5,7,7,5),y=c(3,5,1,3)), hole=FALSE)
   
    Polygcp11 = Polygons(list(Poly1), ID="1")
    Polygcp12 = Polygons(list(Poly4), ID="2")
    Polygcp13 = Polygons(list(Poly6), ID="3")
    Polygcp21 = Polygons(list(Poly1,Poly4), ID="1")
    Polygcp22 = Polygons(list(Poly6), ID="2")
    Polygcp31 = Polygons(list(Poly1), ID="1")
    Polygcp32 = Polygons(list(Poly4,Poly6), ID="2")
    Polygcp4  = Polygons(list(Poly1,Poly4,Poly6), ID="1")
    
    Polygcph11 = Polygons(list(Poly1,Poly2), ID="1")
    Polygcph12 = Polygons(list(Poly4,Poly5), ID="2")
    Polygcph13 = Polygons(list(Poly6), ID="3")
    Polygcph21 = Polygons(list(Poly1,Poly2,Poly4,Poly5), ID="1")
    Polygcph22 = Polygons(list(Poly6), ID="2")
    Polygcph31 = Polygons(list(Poly1,Poly2), ID="1")
    Polygcph32 = Polygons(list(Poly4,Poly5,Poly6), ID="2")

    spgcp1  = SpatialPolygons( list(Polygcp11,Polygcp12,Polygcp13) )
    spgcp2  = SpatialPolygons( list(Polygcp21,Polygcp22) )
    spgcp3  = SpatialPolygons( list(Polygcp31,Polygcp32) )
    spgcp4  = SpatialPolygons( list(Polygcp4) )
    spgcph1 = SpatialPolygons( list(Polygcph11,Polygcph12,Polygcph13) )
    spgcph2 = SpatialPolygons( list(Polygcph21,Polygcph22) )
    spgcph3 = SpatialPolygons( list(Polygcph31,Polygcph32) )
    
    attr(spgcp1@polygons[[1]],"comment") = "0"
    attr(spgcp1@polygons[[2]],"comment") = "0"
    attr(spgcp1@polygons[[3]],"comment") = "0"
    
    attr(spgcp2@polygons[[1]],"comment") = "0 0"
    attr(spgcp2@polygons[[2]],"comment") = "0"
    
    attr(spgcp3@polygons[[1]],"comment") = "0"
    attr(spgcp3@polygons[[2]],"comment") = "0 0"
    
    attr(spgcp4@polygons[[1]],"comment") = "0 0 0"
    
    attr(spgcph1@polygons[[1]],"comment") = "0 1"
    attr(spgcph1@polygons[[2]],"comment") = "0 1"
    attr(spgcph1@polygons[[3]],"comment") = "0"
    
    attr(spgcph2@polygons[[1]],"comment") = "0 1 0 3"
    attr(spgcph2@polygons[[2]],"comment") = "0"
    
    attr(spgcph3@polygons[[1]],"comment") = "0 1"
    attr(spgcph3@polygons[[2]],"comment") = "0 1 0"
    
    expect_that( gcp1 , is_identical_to(spgcp1) )
    expect_that( gcp2 , is_identical_to(spgcp2) )
    expect_that( gcp3 , is_identical_to(spgcp3) )
    expect_that( gcp4 , is_identical_to(spgcp4) )
    expect_that( gcph1, is_identical_to(spgcph1) )
    expect_that( gcph2, is_identical_to(spgcph2) )
    expect_that( gcph3, is_identical_to(spgcph3) )
    
    expect_that( spgcp1 , is_identical_to( doubletranslate(spgcp1)))
    expect_that( spgcp2 , is_identical_to( doubletranslate(spgcp2)))
    expect_that( spgcp3 , is_identical_to( doubletranslate(spgcp3)))
    expect_that( spgcp4 , is_identical_to( doubletranslate(spgcp4)))
    expect_that( spgcph1, is_identical_to( doubletranslate(spgcph1)))
    expect_that( spgcph2, is_identical_to( doubletranslate(spgcph2)))
    expect_that( spgcph3, is_identical_to( doubletranslate(spgcph3)))
    
    expect_that( gcp1 , is_identical_to( doubletranslate(gcp1)))
    expect_that( gcp2 , is_identical_to( doubletranslate(gcp2)))
    expect_that( gcp3 , is_identical_to( doubletranslate(gcp3)))
    expect_that( gcp4 , is_identical_to( doubletranslate(gcp4)))
    expect_that( gcph1, is_identical_to( doubletranslate(gcph1)))
    expect_that( gcph2, is_identical_to( doubletranslate(gcph2)))
    expect_that( gcph3, is_identical_to( doubletranslate(gcph3)))
    
})
