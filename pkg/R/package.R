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
#' \dontrun{.CallOctave('help', 'help')}
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

R.exec <- function(...){	
	system(paste(file.path(R.home(), 'bin', 'R'),' ', ..., sep=''))
}

R.CMD <- function(cmd, ...){
	R.exec('CMD ', cmd, ' ', ...)
}

R.SHLIB <- function(libname, ...){
	R.CMD('SHLIB', '-o ', libname, .Platform$dynlib.ext, ...)
}

#' Compile Source Files from a Development Package
#' 
#' @param pkg the name of the package to compile
#' @param load a logical indicating whether the compiled library should be loaded
#' after the compilation (default) or not.
#' 
#' @return None
#' @keywords internal
compile_src <- function(pkg, load=TRUE){
	
	library(devtools)
	p <- as.package(pkg)
	owd <- getwd()
	on.exit(setwd(owd))
	
	# Compile code in /src
	srcdir <- file.path(p$path, 'src')
	if( file.exists(srcdir) ){
		setwd(srcdir)
		Sys.setenv(R_PACKAGE_DIR=packagePath())
		R.SHLIB(pkg, " *.cpp ")
		if( load )
			load_c(pkg)
	}
}

.LOCAL_PKG_NAME <- 'RcppOctave'

#' Internal Utilities for Package Development
#' 
#' \code{packageEnv} returns the package's environment, which is its namespace 
#' in the case of an installed package, or its devtools environment.
#' 
#' @rdname devutils
#' @return an environment
#' @keywords internal
packageEnv <- function(){ parent.env(environment()) }

#' \code{packageName} returns the current package's name.
#' 
#' @rdname devutils
#' @return a character string
packageName <- function(){
	if( exists('.packageName', packageEnv()) && .packageName != 'datasets') 
		.packageName
	else{# dev mode
		p <- as.package(.LOCAL_PKG_NAME)
		p$package
	}
}

#' \code{packagePath} returns the current package's root directory, which is 
#' its installation/loading directory in the case of an installed package, or
#' its source directory served by devtools. 
#' 
#' @rdname devutils
#' @return a character string
packagePath <- function(){
	if( exists('.packageName', packageEnv()) && .packageName != 'datasets') 
		system.file(package=.packageName)
	else{# dev mode
		p <- as.package(.LOCAL_PKG_NAME)
		p$path
	}
}

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
			assign('.OctaveConfig', conf, packageEnv())
			
			# add a configuration variable for the module path
			.OctaveConfig$modules <<- file.path(packagePath(), 'modules')
		}
				
		return(.OctaveConfig)
	}
		
	settings <- .OctaveConfig[[name]]
	file.path(settings, ...)
}

.onLoad <- function(libname, pkgname){
	
	# load Octave configuration
	OctaveConfig()
	
	# load required Octave libraries
	dyn.load(OctaveConfig('lib', 'liboctave.so'))
	dyn.load(OctaveConfig('lib', 'liboctinterp.so'))
	
	# load compiled library normally or in devmode
	if( !missing(libname) ) library.dynam(.LOCAL_PKG_NAME, pkgname, libname)
	else compile_src(.LOCAL_PKG_NAME) # compile source files and load

	# start Octave session
	ostart()
	# load Octave modules
	omodules()	
}

.onUnload <- function(libpath) {
	
	# unload compiled library normally or in devmode
	dlls <- base::getLoadedDLLs()	
	if ( .LOCAL_PKG_NAME %in%  names(dlls) ){
		if( !missing(libpath) )	library.dynam.unload(.LOCAL_PKG_NAME, libpath)
		else dyn.unload(dlls[[.LOCAL_PKG_NAME]][['path']])
	}
}

roctave <- function(end=TRUE, load=TRUE){
	
	if( load )
		load_all(.LOCAL_PKG_NAME)
	if( end ) 
		oend()
	
	# compile source files
	compile_src(.LOCAL_PKG_NAME)
	
	#ostart()	
}
