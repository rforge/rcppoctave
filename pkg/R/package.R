#' Interfacing R with Octave.
#' 
#'
#' \tabular{ll}{
#' Package: \tab RcppOctave\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2011-11-01\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' @author
#' Renaud Gaujoux \email{renaud@@cbio.uct.ac.za}
#'
#' Maintainer: Renaud Gaujoux \email{renaud@@cbio.uct.ac.za}
#' @name RcppOctave
#' @rdname RcppOctave-package
#' @docType package
#' @title Interfacing R with Octave
#' @keywords package
#' @examples
#' 
#' .CallOctave('svd', matrix(1:9, 3))
#' o_help('svd')
#' 
#' @seealso See \code{\link{.CallOctave}}, \code{\link{o_source}}, \code{\link{o_help}}
NULL

#inlineCxxPlugin <- function (...) 
#{
#	includes <- sprintf("%s\n#include <Rcpp.h>\n%s\n\n#ifndef BEGIN_RCPP\n#define BEGIN_RCPP\n#endif\n\n#ifndef END_RCPP\n#define END_RCPP\n#endif\n\nusing namespace Rcpp;\n", 
#			include.before, include.after)
#	list(env = list(PKG_LIBS = paste(libs, Rcpp:::RcppLdFlags())), 
#			includes = includes, LinkingTo = LinkingTo, body = function(x) {
#				sprintf("BEGIN_RCPP\n%s\nEND_RCPP", x)
#			}, Depends = Depends, Makevars = Makevars, Makevars.win = Makevars.win)
#}

#' Cached RcppOctave Configuration Paths
#' 
#' @param name Name of an RcppOctave path variable
#' @param ... extra names to be concatenated to the result with \code{\link{file.path}}.
#' Only used when \code{name} is not missing.
#' @return  a list (if \code{name is missing}) or a single character string.
#' 
#' @keywords internal
#' @export
#' @examples
#' 
#' OctaveConfig()
#' OctaveConfig('lib')
#' OctaveConfig('include')
#' 
OctaveConfig <- function(name, ...){
	
	# return the whole config list if no name is provided
	if( missing(name) ){
		# create the config list at first call
		if( !exists('.OctaveConfig', packageEnv()) ){
			conf <- list(lib=oconfig('OCTLIBDIR')
						, include=oconfig('OCTINCLUDEDIR')
				)
			
			# add a configuration variable for the module path
			conf$modules <- file.path(packagePath(), 'modules')
			
			assign('.OctaveConfig', conf, packageEnv())
		}
				
		return(.OctaveConfig)
	}
		
	settings <- .OctaveConfig[[name]]
	file.path(settings, ...)
}

# Load/Unload Octave Libraries
.OctaveLibs <- function(unload=FALSE){
		
	dyn.fun <- 
	if( !unload ){ # LOAD		
		function(x, dlls){
			if( !x %in%  dlls )
				dyn.load(OctaveConfig('lib', paste(x, .Platform$dynlib.ext, sep='')))
		}
	}else{ #UNLOAD
		function(x, dlls){
			if( x %in%  dlls )
				dyn.unload(OctaveConfig('lib', paste(x, .Platform$dynlib.ext, sep='')))
		}		
	}

	# load/unload required Octave libraries
	octlibs <- c('liboctave', 'liboctinterp')
	sapply(octlibs, dyn.fun, names(base::getLoadedDLLs()))
}

.onLoad <- function(libname, pkgname){
	
	# load Octave configuration
	OctaveConfig()
	
	# load required Octave libraries
	.OctaveLibs()
	
	# load compiled library normally or in devmode
	if( !missing(libname) ) library.dynam(packageName(), pkgname, libname)
	else compile_src() # compile source files and load

	# start Octave session
	ostart()
	# load Octave modules
	omodules()	
}

.onUnload <- function(libpath) {
	
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()
	pname <- packageName()
	if ( pname %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(pname, libpath)
		else dyn.unload(dlls[[pname]][['path']])
	}
	
	# unload required Octave libraries 
	.OctaveLibs(unload=TRUE)
}

