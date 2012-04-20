# dnl Process this file with autoconf to produce a configure script.
AC_INIT(rgeos, 0.2-6, Roger.Bivand@nhh.no)
AC_CONFIG_SRCDIR(src/rgeos.c)

# find R home and set correct compiler + flags
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  AC_MSG_ERROR([cannot determine R_HOME. Make sure you use R CMD INSTALL!])
fi
RBIN="${R_HOME}/bin/R"

# pick all flags for testing from R
: ${CC=`"${RBIN}" CMD config CC`}
: ${CPP=`"${RBIN}" CMD config CPP`}
: ${CFLAGS=`"${RBIN}" CMD config CFLAGS`}
: ${CPPFLAGS=`"${RBIN}" CMD config CPPFLAGS`}
: ${LDFLAGS=`"${RBIN}" CMD config LDFLAGS`}

GEOS_CONFIG="geos-config"

AC_ARG_WITH([geos-config],
    AC_HELP_STRING([--with-geos-config=GEOS_CONFIG],
           [the location of geos-config]),
           [geos_config=$withval])
if test [ -n "$geos_config" ] ; then
    AC_SUBST([GEOS_CONFIG],["${geos_config}"])
fi
echo "geos-config: ${GEOS_CONFIG}"


if test `${GEOS_CONFIG} --version`;
then

	GEOS_CPPFLAGS=`${GEOS_CONFIG} --cflags`
	GEOS_LIBS=`${GEOS_CONFIG} --libs`
        GEOS_CLIBS=`${GEOS_CONFIG} --clibs`
        GEOS_VER=`${GEOS_CONFIG} --version`

else

	echo "Error: geos-config not found"
	echo "The geos-config script distributed with GEOS could not be found."
	echo "If you have not installed the GEOS libraries, you can"
	echo "download the source from  http://http://trac.osgeo.org/geos/"
	echo "If you have installed the GEOS libraries, then make sure that"
	echo "geos-config is in your path. Try typing geos-config at a"
	echo "shell prompt and see if it runs. If not, use:"
	echo " --configure-args='--with-geos-config=/usr/local/bin/geos-config'"	echo "with appropriate values for your installation."
	echo ""

	exit 1

fi

echo "GEOS: $GEOS_VER"
GEOS_VER_DOT=`echo $GEOS_VER | tr -d "."`
if test ${GEOS_VER_DOT} -lt 322 ; then
    echo "Error: GEOS version less than 3.2.2"
    echo "Upgrade GEOS to at least 3.2.2"

    exit 1
fi

AC_SUBST([PKG_CPPFLAGS],["${PKG_CPPFLAGS} ${GEOS_CPPFLAGS}"])
AC_SUBST([PKG_LIBS],["${PKG_LIBS} ${GEOS_LIBS} ${GEOS_CLIBS}"])

# honor PKG_xx overrides
# for CPPFLAGS we will superfluously double R's flags
# since we'll set PKG_CPPFLAGS with this, but that shouldn't hurt
CPPFLAGS="${CPPFLAGS} ${PKG_CPPFLAGS}"
LIBS="${LIBS} ${PKG_LIBS}"

geosok=yes
AC_CHECK_HEADERS(geos_c.h,,geosok=no)
AC_CHECK_LIB(geos_c,initGEOS_r,,geosok=no)
if test "${geosok}" = no; then
   AC_MSG_ERROR([libgeos_c and/or geos_c.h not found in given locations.])
fi

echo "CPPFLAGS: $PKG_CPPFLAGS"
AC_SUBST(PKG_CPPFLAGS)
echo "LIBS: $PKG_LIBS"
AC_SUBST(PKG_LIBS)

if test -e ".svn" ; then
  svnversion -n > inst/SVN_VERSION
fi

AC_CONFIG_FILES(src/Makevars)
AC_OUTPUT

