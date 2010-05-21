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
  
    
    expect_that( p   , is_identical_to(spp) )
    expect_that( mp  , is_identical_to(spmp) )
    expect_that( gcp1, is_identical_to(spmp) )
    expect_that( gcp2, is_identical_to(spmp) )
    expect_that( gcp3, is_identical_to(spmp) )
    expect_that( gcp3, is_identical_to(spmp) )
    expect_that( gcp3, is_identical_to(spmp) )

    
    expect_that( spp , is_identical_to( doubletranslate(spp)))
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
    Line2 = Line(cbind( x=1:4,y=4:1 ))
    
    Linesl = Lines( list(Line1) )
    Linesml1 = Lines( list(Line1, Line1) )
    Linesml2 = Lines( list(Line1, Line2) )
    
    spl    = SpatialLines( list(Linesl) )
    spml1  = SpatialLines( list(Linesml1) )
    spml2  = SpatialLines( list(Linesml2) )
    spgcl1 = SpatialLines( list(Linesl,Linesl) )
    spgcl2 = SpatialLines( list(Linesl,Linesml2,Linesl) )


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
    expect_that( gcl2, is_identical_to( doubletranslate(gcl1) ))

})   



test_that("translate lines", {

    lr1 = readWKT("LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1)")
    lr2 = readWKT("LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1)")
    gclr1 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1), LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1) )")
    gclr2 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 1 2, 2 2, 2 1, 1 1), LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1) )")
    gclr3 = readWKT("GEOMETRYCOLLECTION( LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1), LINEARRING (1 1, 2 1, 2 2, 1 2, 1 1) )")
    
    Line1 = Line(cbind( x=c(1,1,2,2,1),y=c(1,2,2,1,1) ))
    Line2 = Line(cbind( x=c(1,2,2,1,1),y=c(1,1,2,2,1) ))
    
    Lineslr1 = Lines( list(Line1) )
    Lineslr2 = Lines( list(Line1) ) 

    
    splr1   = SpatialLines( list(Lineslr1) )
    splr2   = SpatialLines( list(Lineslr2) )
    spgclr1 = SpatialLines( list(Linesr1,Linesr1) )
    spgclr2 = SpatialLines( list(Linesr1,Linesr2) )
    spgclr3 = SpatialLines( list(Linesr2,Linesr2) )
    

    expect_that( lr1  , is_identical_to(splr1) )
    expect_that( lr2  , is_identical_to(splr1) )
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









