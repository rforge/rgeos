SymDiffGpcGEOS <- function(gpclist1, gpclist2) {
    .Call("SymDiffGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


UnionGpcGEOS <- function(gpclist1, gpclist2) {
    .Call("UnionGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


IntersectGpcGEOS <- function(gpclist1, gpclist2) {
    .Call("IntersectGpcGEOS", gpclist1, gpclist2, PACKAGE="rgeos")
}


checkHolesGPC <- function(gpclist) {
    .Call("checkHolesGPC", gpclist, PACKAGE="rgeos")
}

