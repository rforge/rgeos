# Version 0.5-7 (development, rev. 641-662)

* adapt to OverlayNG in GEOS 3.9

* update Windows static library to GEOS 3.9

* add gMinumumRotatedRectangle() and gMaximumInscribedCircle() for GEOS 3.9

* adapt configure.ac for GEOS 3.10

* add support for structured geometry fixer in gMakeValid() from 3.10

* default `topologyPreserving=FALSE` argument in `gSimplify()` made dependent on GEOS version, < 3.10 `FALSE`, >= 3.10 `TRUE` to get around GEOS returning mixed-dimension collections >= 3.10 if FALSE.

# Version 0.5-5 (2020-09-07, rev. 634-640)

* add gMakeValid() for GEOS 3.8

* update Windows static library to GEOS 3.8

* add gCoverageUnion()

* add stdlib.h to configure.ac

