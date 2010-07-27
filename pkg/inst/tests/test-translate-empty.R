library(testthat)
library(rgeos)

setScale()
context("Translation empty geometries")

test_that("empty geometrycollection", {
	gc1 = readWKT("GEOMETRYCOLLECTION EMPTY")

	expect_that( gc1,  is_identical_to(doubletranslate(gc1)) )
})


test_that("empty points", {
    p1 = readWKT("POINT EMPTY")
    p2 = readWKT("MULTIPOINT EMPTY")
    p3 = readWKT("GEOMETRYCOLLECTION(POINT EMPTY)")
    p4 = readWKT("GEOMETRYCOLLECTION(MULTIPOINT EMPTY)")
    
    expect_that( p1,  is_identical_to(p2) )
    expect_that( p2,  is_identical_to(p3) )
    expect_that( p3,  is_identical_to(p4) )
    
    expect_that( p1,  is_identical_to(doubletranslate(p1)) )
    expect_that( p2,  is_identical_to(doubletranslate(p2)) )
    expect_that( p3,  is_identical_to(doubletranslate(p3)) )
    expect_that( p4,  is_identical_to(doubletranslate(p4)) )
    
    pg1 = readWKT("GEOMETRYCOLLECTION(POINT EMPTY,POINT EMPTY)")
    pg2 = readWKT("GEOMETRYCOLLECTION(POINT EMPTY,MULTIPOINT EMPTY)")
    pg3 = readWKT("GEOMETRYCOLLECTION(MULTIPOINT EMPTY,POINT EMPTY)")
    pg4 = readWKT("GEOMETRYCOLLECTION(MULTIPOINT EMPTY,MULTIPOINT EMPTY)")

    pg5 = readWKT("GEOMETRYCOLLECTION(POINT (1 1),POINT EMPTY)")
    pg6 = readWKT("GEOMETRYCOLLECTION(POINT (1 1),MULTIPOINT EMPTY)")
    pg7 = readWKT("GEOMETRYCOLLECTION(MULTIPOINT (1 1),POINT EMPTY)")
    pg8 = readWKT("GEOMETRYCOLLECTION(MULTIPOINT (1 1),MULTIPOINT EMPTY)")

    pg9 = readWKT("GEOMETRYCOLLECTION(POINT EMPTY,POINT (1 1))")
    pg10= readWKT("GEOMETRYCOLLECTION(POINT EMPTY,MULTIPOINT (1 1))")
    pg11= readWKT("GEOMETRYCOLLECTION(MULTIPOINT EMPTY,POINT (1 1))")
    pg12= readWKT("GEOMETRYCOLLECTION(MULTIPOINT EMPTY,MULTIPOINT (1 1))")

    expect_that( pg1,  is_identical_to(pg2) )
    expect_that( pg2,  is_identical_to(pg3) )
    expect_that( pg3,  is_identical_to(pg4) )

    expect_that( pg5,  is_identical_to(pg5) )
    expect_that( pg6,  is_identical_to(pg7) )
    expect_that( pg7,  is_identical_to(pg8) )
    
    expect_that( pg9,  is_identical_to(pg10) )
    expect_that( pg10, is_identical_to(pg11) )
    expect_that( pg11, is_identical_to(pg12) )
    
    expect_that( pg1,  is_identical_to(doubletranslate(pg1)) )
    expect_that( pg2,  is_identical_to(doubletranslate(pg2)) )
    expect_that( pg3,  is_identical_to(doubletranslate(pg3)) )
    expect_that( pg4,  is_identical_to(doubletranslate(pg4)) )
    expect_that( pg5,  is_identical_to(doubletranslate(pg5)) )
    expect_that( pg6,  is_identical_to(doubletranslate(pg6)) )
    expect_that( pg7,  is_identical_to(doubletranslate(pg7)) )
    expect_that( pg8,  is_identical_to(doubletranslate(pg8)) )
    expect_that( pg9,  is_identical_to(doubletranslate(pg9)) )
    expect_that(pg10,  is_identical_to(doubletranslate(pg10)) )
    expect_that(pg11,  is_identical_to(doubletranslate(pg11)) )
    expect_that(pg12,  is_identical_to(doubletranslate(pg12)) )
    
})

test_that("empty linestrings", {

    l1 = readWKT("LINESTRING EMPTY")
    l2 = readWKT("MULTILINESTRING EMPTY")
    l3 = readWKT("GEOMETRYCOLLECTION(LINESTRING EMPTY)")
    l4 = readWKT("GEOMETRYCOLLECTION(MULTILINESTRING EMPTY)")
    
    expect_that( l1, is_identical_to(l2) )
    expect_that( l2, is_identical_to(l3) )
    expect_that( l3, is_identical_to(l4) )
    
    expect_that( l1, is_identical_to(doubletranslate(l1)) )
    expect_that( l2, is_identical_to(doubletranslate(l2)) )
    expect_that( l3, is_identical_to(doubletranslate(l3)) )
    expect_that( l4, is_identical_to(doubletranslate(l4)) )
    
    
    ml1 = readWKT("MULTILINESTRING((1 1,2 2), EMPTY)")
    ml2 = readWKT("MULTILINESTRING(EMPTY, (1 1,2 2))")
    
    expect_that( ml1, is_identical_to(doubletranslate(ml1)) )
    expect_that( ml2, is_identical_to(doubletranslate(ml2)) )
    
    
    lg1 = readWKT("GEOMETRYCOLLECTION(LINESTRING EMPTY,LINESTRING EMPTY)")
    lg2 = readWKT("GEOMETRYCOLLECTION(MULTILINESTRING EMPTY,MULTILINESTRING EMPTY)")
    lg3 = readWKT("GEOMETRYCOLLECTION(LINESTRING EMPTY,MULTILINESTRING EMPTY)")
    lg4 = readWKT("GEOMETRYCOLLECTION(MULTILINESTRING EMPTY,LINESTRING EMPTY)")
    
    lg5 = readWKT("GEOMETRYCOLLECTION(LINESTRING EMPTY,LINESTRING(1 1,2 2))")
    lg6 = readWKT("GEOMETRYCOLLECTION(MULTILINESTRING EMPTY,MULTILINESTRING((1 1,2 2)))")
    lg7 = readWKT("GEOMETRYCOLLECTION(LINESTRING EMPTY,MULTILINESTRING((1 1,2 2)))")
    lg8 = readWKT("GEOMETRYCOLLECTION(MULTILINESTRING EMPTY,LINESTRING(1 1,2 2))")
    
    lg9 = readWKT("GEOMETRYCOLLECTION(LINESTRING(1 1,2 2),LINESTRING EMPTY)")
    lg10= readWKT("GEOMETRYCOLLECTION(MULTILINESTRING((1 1,2 2)),MULTILINESTRING EMPTY)")
    lg11= readWKT("GEOMETRYCOLLECTION(LINESTRING(1 1,2 2),MULTILINESTRING EMPTY)")
    lg12= readWKT("GEOMETRYCOLLECTION(MULTILINESTRING((1 1,2 2)),LINESTRING EMPTY)")

    expect_that( lg1,  is_identical_to(lg2) )
    expect_that( lg2,  is_identical_to(lg3) )
    expect_that( lg3,  is_identical_to(lg4) )

    expect_that( lg5,  is_identical_to(lg5) )
    expect_that( lg6,  is_identical_to(lg7) )
    expect_that( lg7,  is_identical_to(lg8) )
    
    expect_that( lg9,  is_identical_to(lg10) )
    expect_that( lg10, is_identical_to(lg11) )
    expect_that( lg11, is_identical_to(lg12) )
    
    expect_that( lg1, is_identical_to(doubletranslate(lg1)) )
    expect_that( lg2, is_identical_to(doubletranslate(lg2)) )
    expect_that( lg3, is_identical_to(doubletranslate(lg3)) )
    expect_that( lg4, is_identical_to(doubletranslate(lg4)) )
    expect_that( lg5, is_identical_to(doubletranslate(lg5)) )
    expect_that( lg6, is_identical_to(doubletranslate(lg6)) )
    expect_that( lg7, is_identical_to(doubletranslate(lg7)) )
    expect_that( lg8, is_identical_to(doubletranslate(lg8)) )
    expect_that( lg9, is_identical_to(doubletranslate(lg9)) )
    expect_that( lg10, is_identical_to(doubletranslate(lg10)) )
    expect_that( lg11, is_identical_to(doubletranslate(lg11)) )
    expect_that( lg12, is_identical_to(doubletranslate(lg12)) )
})


test_that("empty linearrings", {

    lr1 = readWKT("LINEARRING EMPTY")
    lr2 = readWKT("GEOMETRYCOLLECTION(LINEARRING EMPTY)")
    
    expect_that( lr1, is_identical_to(lr2) )
    
    expect_that( lr1, is_identical_to(doubletranslate(lr1)) )
    expect_that( lr2, is_identical_to(doubletranslate(lr2)) )
    
    lrg1 = readWKT("GEOMETRYCOLLECTION(LINEARRING EMPTY,LINEARRING EMPTY)")
    lrg2 = readWKT("GEOMETRYCOLLECTION(LINEARRING EMPTY,LINEARRING(1 1,2 2,3 1,1 1))")
    lrg3 = readWKT("GEOMETRYCOLLECTION(LINEARRING(1 1,2 2,3 1,1 1),LINEARRING EMPTY)")

    expect_that( lrg1, is_identical_to(doubletranslate(lrg1)) )
    expect_that( lrg2, is_identical_to(doubletranslate(lrg2)) )
    expect_that( lrg3, is_identical_to(doubletranslate(lrg3)) )

})


test_that("empty polygons", {

    p1 = readWKT("POLYGON EMPTY")
    p2 = readWKT("MULTIPOLYGON EMPTY")
    p3 = readWKT("GEOMETRYCOLLECTION(POLYGON EMPTY)")
    p4 = readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON EMPTY)")
    
    expect_that( p1, is_identical_to(p2) )
    expect_that( p2, is_identical_to(p3) )
    expect_that( p3, is_identical_to(p4) )
    
    expect_that( p1, is_identical_to(doubletranslate(p1)) )
    expect_that( p2, is_identical_to(doubletranslate(p2)) )
    expect_that( p3, is_identical_to(doubletranslate(p3)) )
    expect_that( p4, is_identical_to(doubletranslate(p4)) )
    
    
    mp1 = readWKT("MULTIPOLYGON(((1 1,2 2,3 1,1 1)), EMPTY)")
    mp2 = readWKT("MULTIPOLYGON(EMPTY, ((1 1,2 2,3 1,1 1)))")
    
    expect_that( mp1, is_identical_to(doubletranslate(mp1)) )
    expect_that( mp2, is_identical_to(doubletranslate(mp2)) )
    
    
    pg1 = readWKT("GEOMETRYCOLLECTION(POLYGON EMPTY,POLYGON EMPTY)")
    pg2 = readWKT("GEOMETRYCOLLECTION(POLYGON EMPTY,MULTIPOLYGON EMPTY)")
    pg3 = readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON EMPTY,POLYGON EMPTY)")
    pg4 = readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON EMPTY,MULTIPOLYGON EMPTY)")
    
    pg5 = readWKT("GEOMETRYCOLLECTION(POLYGON EMPTY, POLYGON((1 1,2 2,3 1,1 1)) )")
    pg6 = readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON EMPTY, POLYGON((1 1,2 2,3 1,1 1)) )")
    pg7 = readWKT("GEOMETRYCOLLECTION(POLYGON EMPTY, MULTIPOLYGON(((1 1,2 2,3 1,1 1))) )")
    pg8 = readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON EMPTY, MULTIPOLYGON(((1 1,2 2,3 1,1 1))) )")

    pg9 = readWKT("GEOMETRYCOLLECTION(POLYGON((1 1,2 2,3 1,1 1)), POLYGON EMPTY)")
    pg10= readWKT("GEOMETRYCOLLECTION(POLYGON((1 1,2 2,3 1,1 1)), MULTIPOLYGON EMPTY)")
    pg11= readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON(((1 1,2 2,3 1,1 1))), POLYGON EMPTY)")
    pg12= readWKT("GEOMETRYCOLLECTION(MULTIPOLYGON(((1 1,2 2,3 1,1 1))), MULTIPOLYGON EMPTY)")

    expect_that( pg1, is_identical_to(pg2) )
    expect_that( pg2, is_identical_to(pg3) )
    expect_that( pg3, is_identical_to(pg4) )
    
    expect_that( pg5, is_identical_to(pg6) )
    expect_that( pg6, is_identical_to(pg7) )
    expect_that( pg7, is_identical_to(pg8) )

    expect_that( pg9, is_identical_to(pg10) )
    expect_that(pg10, is_identical_to(pg11) )
    expect_that(pg11, is_identical_to(pg12) )

    expect_that( pg1, is_identical_to(doubletranslate(pg1)) )
    expect_that( pg2, is_identical_to(doubletranslate(pg2)) )
    expect_that( pg3, is_identical_to(doubletranslate(pg3)) )
    expect_that( pg4, is_identical_to(doubletranslate(pg4)) )
    expect_that( pg5, is_identical_to(doubletranslate(pg5)) )
    expect_that( pg6, is_identical_to(doubletranslate(pg6)) )
    expect_that( pg7, is_identical_to(doubletranslate(pg7)) )
    expect_that( pg8, is_identical_to(doubletranslate(pg8)) )
    expect_that( pg9, is_identical_to(doubletranslate(pg9)) )
    expect_that( pg10,is_identical_to(doubletranslate(pg10)))
    expect_that( pg11,is_identical_to(doubletranslate(pg11)))
    expect_that( pg12,is_identical_to(doubletranslate(pg12)))

})