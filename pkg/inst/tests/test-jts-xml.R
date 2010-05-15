library(testthat)
library(XML)
library(rgeos)

# case-insensitive get function
fuzzyget = function(x, pos = -1, envir = as.environment(pos), mode = "any", inherits = TRUE) {
    
    obj = objects(envir=envir,all=TRUE)
    
    if(x %in% obj)
        get(x, pos, envir, mode, inherits)
        
    w = which( tolower(x) == tolower(obj) )
    if (length(w) != 1)
        return(NULL)
    
    return( get(obj[w], pos, envir, mode, inherits) )
}

processArg = function(x) {
    
    return(x)
}

# Some functions have different names between GEOS and JTS
funcTranslate = list (  "getboundary" = "Boundary",
                        "getInteriorPoint" = "PointOnSurface") # Unsure about the equivalence


xmldir = 'tests/testxml'
testdirs = list.files(system.file(xmldir,package="rgeos"))

for (d in testdirs) {
    testfiles = list.files(system.file(file.path(xmldir,d),package="rgeos")) 
    
    for (f in testfiles)  {
        xmlfile =  system.file(file.path(xmldir,d,f),package="rgeos")

        context(f)
        x = xmlRoot(xmlTreeParse(xmlfile,ignoreBlanks=TRUE))
        nodes = xmlSApply(x,xmlName)

        test_that("valid node types",{
            validNodeTypes = c("precisionModel","case","comment")
            expect_that( all(nodes %in% validNodeTypes), is_true() )
        })
        
        #Handle precisionModel nodes - only use the first model
        pmAttrs =  xmlAttrs( x[[ which(nodes == "precisionModel")[1] ]] )

        test_that("precisionModel attribute tests", {
            expect_that( length(pmAttrs) == 1 | length(pmAttrs) == 3, is_true() )
        
            if (length(pmAttrs) == 1) {
                type = pmAttrs[["type"]]
            } else if (length(pmAttrs) == 3) {
                setScale(as.numeric( pmAttrs[["scale"]] ))

                expect_that( pmAttrs[["offsetx"]], equals("0.0") )
                expect_that( pmAttrs[["offsety"]], equals("0.0") )
            } 
        })

        #Handle case nodes
        for ( i in which(nodes == "case") ) {
            caseNodes = xmlSApply(x[[i]],xmlName)

            whichDesc = which(caseNodes == "desc")
            whichTests = which(caseNodes == "test")

            desc = xmlValue( x[[i]][[ whichDesc[1] ]] )

            whichArgs = which(caseNodes != "desc" & caseNodes != "test")
            
            args = rep( NA,length(whichArgs) )
            # argument nodes can either contain the value or have a file attribute
            for ( j in whichArgs) {
                if (is.null( xmlAttrs(x[[i]][[j]]) )) {
                    args[[ xmlName(x[[i]][[j]]) ]] = processArg(xmlValue(x[[i]][[j]]))
                } else {
                    file = xmlAttrs(x[[i]][[j]])[["file"]]
                    l = paste( readLines(file), collapse="" )
                    args[[ xmlName(x[[i]][[j]]) ]] = processArg(l)
                }
            }
            
            #make sure the arg names are lowercase for the sake of consistency
            names(args) = tolower(names(args))
            

            for ( j in whichTests ) {
                
                test_that(paste(desc,'- test nodes in proper format') , {
                    expect_that( xmlSize( x[[i]][[j]] ), equals(1) )
                    expect_that( xmlName( x[[i]][[j]][[1]] ), equals("op") )
                })
                
                if ( xmlSize( x[[i]][[j]] ) == 1 & xmlName( x[[i]][[j]][[1]] ) == "op" ) {

                    opAttrs = xmlAttrs( x[[i]][[j]][[1]] )
                    opReturn = xmlValue( x[[i]][[j]][[1]] )
                    opNArgs = length(opAttrs)-1
                    
                    opName = opAttrs[['name']]
                    if ( !is.null(funcTranslate[[opName]]) )
                        opName = funcTranslate[[opName]]
                    
                    funcPtr = fuzzyget( paste("RGEOS",opName,sep=''),
                                        envir=as.environment("package:rgeos") )
                    
                    test_that(paste(desc,'-',opName), {
                        expect_that( is.null(funcPtr), is_false() )
                    
                    
                        if ( !is.null(funcPtr) ) {
                            funcNArgs = length( formals(funcPtr) )
                            funcArgs = list()
                            for (argi in 1:funcNArgs) {

                                argName = opAttrs[[ paste("arg",argi,sep='') ]]

                                if ( tolower(argName) %in% names(args) ) {
                                    funcArgs[[argi]] = args[[ tolower(argName) ]]
                                } else {
                                    funcArgs[[argi]] = argName
                                }

                            }

                            funcReturn = do.call(funcPtr, funcArgs, envir=as.environment("package:rgeos"))
                            print(opReturn)
                        }
                    }) 
                }                
            } 
        }
    }
}

