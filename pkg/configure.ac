## Process this file with autoconf to produce a configure script.
##
## Configure.in for RcppOctave
##
## Borrowed and adapted from the file configure.in for RcppGSL
## Copyright (C) 2010 Romain Francois and Dirk Eddelbuettel
## Licensed under GNU GPL 2 or later
##
## Copyright (C) 2011 Renaud Gaujoux

AC_INIT([RcppOctave], 0.14.5)

# You will very likely need to ensure that the same C compiler and compiler 
# flags are used in the configure tests as when compiling R or your package. 
# Under a Unix-alike, you can achieve this by including the following fragment 
# early in configure.ac
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "RcppOctave - configure: could not determine R_HOME"
  exit 1
fi

# get compiler and compilation flags from R
if test -n "${R_HOME}"; then
  AC_MSG_CHECKING([R CC])
  CC=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CC`
  AC_MSG_RESULT(${CC})
  
  AC_MSG_CHECKING([R CFLAGS])
  CFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CFLAGS`
  AC_MSG_RESULT(${CFLAGS})
  
  AC_MSG_CHECKING([R CPPFLAGS])
  CPPFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CPPFLAGS`
  AC_MSG_RESULT(${CPPFLAGS})
  
  AC_MSG_CHECKING([R CXXFLAGS])
  CXXFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config CXXFLAGS`
  AC_MSG_RESULT(${CXXFLAGS})
fi

# Now checks for compiler using default macros
AC_PATH_TOOL([R_CC_COMPILER], [${CC}])
AC_PROG_CC
AC_PROG_CXX
AC_PATH_TOOL([R_CXX_COMPILER], [${CXX}])
AC_LANG(C++)


# check R is a shared library
R_LDFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config --ldflags 2>/dev/null`
R_CPPFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/R" CMD config --cppflags 2>/dev/null`

AC_MSG_RESULT([Original R_LDFLAGS: ${R_LDFLAGS}])
AC_MSG_RESULT([Original R_CPPFLAGS: ${R_CPPFLAGS}])

# remove -fopenmp flag that breaks mkoctfile (effectively used when R shared library)
R_LDFLAGS=`echo $R_LDFLAGS | sed 's/-fopenmp//'`

has_R_shlib=no
if test -n "$R_LDFLAGS"; then
   has_R_shlib=yes
fi

AC_MSG_CHECKING([whether R is a shared library])
if test "${has_R_shlib}" = no; then
	AC_MSG_RESULT(no)
	# add path to R headers 
	R_CPPFLAGS="-I'${R_INCLUDE_DIR}' -I'${R_INCLUDE_DIR}${R_ARCH}'"
else
   AC_MSG_RESULT(yes)
fi

# Mac OS support
# inspired/partially borrowed from package rgl
AC_MSG_CHECKING([type of Operating System])
OStype=`uname`
AC_MSG_RESULT(${OStype})

AC_MSG_CHECKING([whether OS is Mac OS (Darwin)])
if test "$OStype" == "Darwin" ; then
  darwin="yes"
  
  ## adapt R_LDFLAGS aimed at mkoctfile: pass them via flag -Wl
  ## multiple options must be separated by commas
  R_LDFLAGS=-Wl,"\"$R_LDFLAGS\""
  
  ## we want the *build* cputype and not the host one.
  cmd=`echo $CC $CFLAGS | grep -E 'x86_64|ppc64|-m64'`
  if test -n "$cmd"; then
    have_64bit="yes"
  else
    have_64bit="no"
  fi 
else
  darwin="no"
fi
AC_MSG_RESULT(${darwin})

AC_MSG_RESULT([Using mkoctfile with R_LDFLAGS: ${R_LDFLAGS}])
AC_MSG_RESULT([Using mkoctfile with R_CPPFLAGS: ${R_CPPFLAGS}])

# pass R flags down to makefile in modules/
AC_SUBST(R_CPPFLAGS)
AC_SUBST(R_LDFLAGS)

#RCPP_LIBS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e "Rcpp:::LdFlags()"`
#RCPP_CXXFLAGS=`"${R_HOME}/bin${R_ARCH_BIN}/Rscript" -e "Rcpp:::CxxFlags()"`
#AC_MSG_RESULT([Using Rcpp LIBS: ${RCPP_LIBS}])
#AC_MSG_RESULT([Using Rcpp CXXFLAGS: ${R_CXXFLAGS}])
#AC_SUBST(RCPP_LIBS)
#AC_SUBST(RCPP_CXXFLAGS)


## Use octave-config to find arguments for compiler and linker flags
##
## Check for non-standard programs: 
## octave-config

## Look up for octave binary path from a configure option
## or an environment variable
AC_ARG_WITH([octave],
            AC_HELP_STRING([--with-octave=PATH],
                           [the location of octave root directory, binary file or bin/ sub-directory]),
            [octave_path=$withval])

AC_MSG_CHECKING([Octave binary path specification])
if test [ -n "$octave_path" ] ; then # passed as an option
  AC_MSG_RESULT([$octave_path [[configure option]]])
elif test [ -n "${OCTAVE_PATH}" ] ; then
  octave_path="${OCTAVE_PATH}"
  AC_MSG_RESULT([$octave_path [[environment]]])
else
  AC_MSG_RESULT([none])
fi

if test [ -n "$octave_path" ] ; then
	if test [ -f "$octave_path" ] ; then # path is a file: use parent directory
	  OCTAVE_BINDIR=`AS_DIRNAME(["$octave_path"])`
	elif test [ -d "$octave_path" ] ; then # path is a directory
      if test [ -f "$octave_path/octave-config" ] ; then # path contains octave-config
   	    OCTAVE_BINDIR="$octave_path"
   	  elif test [ -f "$octave_path/bin/octave-config" ] ; then # path contains bin/ sub-directory
   	  	OCTAVE_BINDIR="$octave_path/bin"
   	  fi
	fi
    # error if the path specification did not provide the correct information
    if test [ -z "${OCTAVE_BINDIR}" ] ; then
	  AC_MSG_ERROR([invalid Octave path specification. Must be either:
	    * path to Octave root directory;
	    * path to Octave bin directory;
	    * path to Octave binary file.
	  ])
    fi
	AC_MSG_RESULT([using Octave binary path ... ${OCTAVE_BINDIR}])
	
	# looking up octave-config and mkoctfile
	AC_PATH_PROG([OCTAVE_CONFIG], [octave-config], [], ["${OCTAVE_BINDIR}"])
	AC_PATH_PROG([OCTAVE_MKOCTFILE], [mkoctfile], [], ["${OCTAVE_BINDIR}"])
else
  AC_MSG_RESULT([using Octave binary path from \$PATH])
  # looking up octave-config and mkoctfile
  AC_PATH_PROG([OCTAVE_CONFIG], [octave-config])
  AC_PATH_PROG([OCTAVE_MKOCTFILE], [mkoctfile])
fi

## If octave-config was found, let's use it
if test "${OCTAVE_MKOCTFILE}" != ""; then	

	AC_DEFUN([AC_OCTAVE_MKCONFIG],[ 
		AC_MSG_CHECKING([Octave $3])
		ac_mkoct_var_$2=`${OCTAVE_MKOCTFILE} --print $2`
		AC_SUBST([$1], [$ac_mkoct_var_$2])
		AC_MSG_RESULT([$$1])
	])
	
	AC_DEFUN([AC_OCTAVE_CONFIG],[ 
		AC_MSG_CHECKING([Octave $3])
		ac_octave_var_$2=`${OCTAVE_CONFIG} --print $2`
		AC_SUBST([$1], [$ac_octave_var_$2])
		AC_MSG_RESULT([$$1])
	])
	
	# specify if one uses a custom path to Octave
    if test [ -n "$octave_path" ] ; then
      OCTAVE_CUSTOMED=TRUE
    else 
      OCTAVE_CUSTOMED=FALSE
    fi
    AC_SUBST(OCTAVE_CUSTOMED)
    
	AC_OCTAVE_CONFIG([OCTAVE_BINDIR], [BINDIR], [Octave bin directory])
	AC_OCTAVE_CONFIG([OCTAVE_LIBDIR], [OCTLIBDIR], [Octave libraries directory])
    
	# Use mkoctfile for compiler and linker arguments
	AC_OCTAVE_MKCONFIG([OCT_CPPFLAGS], [CPPFLAGS], [cpp flags])
    AC_OCTAVE_MKCONFIG([OCT_INCFLAGS], [INCFLAGS], [include flags])
    OCT_CPPFLAGS+=" ${OCT_INCFLAGS}"
    OCT_LDFLAGS=" ${OCTAVE_LFLAGS} ${OCTAVE_LIBS}"
    AC_OCTAVE_MKCONFIG([OCTAVE_LFLAGS], [LFLAGS], [-L flags])
    AC_OCTAVE_MKCONFIG([OCTAVE_LIBS], [OCTAVE_LIBS], [libraries])
    # add to LDFLAGS
    OCT_LDFLAGS=" ${OCTAVE_LFLAGS} ${OCTAVE_LIBS}"
    
    # check Octave compilers
    AC_OCTAVE_MKCONFIG([OCTAVE_CC], [CC], [C compiler])
    AC_OCTAVE_MKCONFIG([OCTAVE_F77], [F77], [Fortran compiler])
    
    # Octave config info
    AC_OCTAVE_CONFIG([OCTAVE_PLATFORM], [CANONICAL_HOST_TYPE], [platform])
    AC_OCTAVE_CONFIG([OCTAVE_API_VERSION], [API_VERSION], [API version])
    
    # check Octave version to know whether one should infer output names:
    # it works for Octave >= 3.4.3
    AC_OCTAVE_CONFIG([OCTAVE_VERSION], [VERSION], [version])
    
    CMPV="3.4.3"
    AC_MSG_CHECKING([whether to infer output names for Octave function (>= ${CMPV})])
    AS_VERSION_COMPARE(${OCTAVE_VERSION}, ${CMPV}, [OCT_POST_3_4_0=-1], [OCT_POST_3_4_0=0], [OCT_POST_3_4_0=1])    
    AS_IF([test "${OCT_POST_3_4_0}" != "-1"], AC_MSG_RESULT([yes]), AC_MSG_RESULT([no]))
    # add to CPPFLAGS
    OCT_CPPFLAGS+=" -DOCT_POST_3_4_0=${OCT_POST_3_4_0}"

elif test "$darwin" == "no" ; then
    AC_MSG_ERROR([mkoctfile not found, is Octave installed?
    
    -> On Debian/Ubuntu please check that the required packages are installed:
    
      * For Octave 3.6 (works out of the box):
        sudo apt-get install octave liboctave-dev
        # install as usual in R
        install.packages('RcppOctave')
        
      * For Octave 3.2.4 (might require extra command)
        sudo apt-get install octave3.2 octave3.2-headers
        # requires to explicitly export Octave lib directory 
        export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:`octave-config -p OCTLIBDIR`
        # install as usual in R
        install.packages('RcppOctave')
    ])
else
	AC_MSG_ERROR([mkoctfile not found, is Octave installed?
    
    -> On Mac OS please check that the required packages are installed:
    
      * octave package from homebrew (works out of the box):
        $ brew install gfortran octave
        
        # install as usual in R
        > install.packages('RcppOctave')
    ])
fi

# Now substitute these variables in src/Makevars.in to create src/Makevars
AC_MSG_RESULT([Using Octave LDFLAGS: ${OCT_LDFLAGS}])
AC_SUBST(OCT_LDFLAGS)
AC_MSG_RESULT([Using Octave CPPFLAGS: ${OCT_CPPFLAGS}])
AC_SUBST(OCT_CPPFLAGS)

# Module path
AC_MSG_CHECKING([RcppOctave Octave module directory])
OCT_MODULES_PATH=${R_PACKAGE_DIR}/modules  
AC_MSG_RESULT([${OCT_MODULES_PATH}])
AC_SUBST(OCT_MODULES_PATH)

# Configure libRrng
#AC_CONFIG_SUBDIRS([src/modules])

AC_OUTPUT(R/config-vars.R)
AC_OUTPUT(src/Makevars)
AC_OUTPUT(src/modules/Makefile)

