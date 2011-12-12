.LOCAL_PKG_NAME <- 'RcppOctave'

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
	
	if( !missing(pkg) ){
		library(devtools)
		p <- as.package(pkg)
		path <- p$path
	}else{
		pkg <- packageName()
		path <- packagePath()
	}
	
	owd <- getwd()
	on.exit(setwd(owd))
	
	# Compile code in /src
	srcdir <- file.path(path, 'src')
	if( file.exists(srcdir) ){
		cat("# DEVMODE: Compiling src/ ... ")		
		setwd(srcdir)
		Sys.setenv(R_PACKAGE_DIR=path)
		R.SHLIB(pkg, " *.cpp ")
		cat("OK\n")
		if( load )
			load_c(pkg)
	}
}

#' Internal Utilities for Package Development
#' 
#' \code{packageEnv} returns the package's environment, which is its namespace 
#' in the case of an installed package, or its devtools environment.
#' 
#' @rdname devutils
#' @return an environment
#' @keywords internal
packageEnv <- function(){ parent.env(environment()) }

#' \code{getLoadingNamespace} returns information about the loading namespace.
#' It is a wrapper to \code{\link{loadingNamespaceInfo}}, that does not throw 
#' an error.
#' 
#' @param env logical that indicates that the namespace's environment (i.e. the 
#' namespace itself) should be returned.
#' @param info logical that indicate that the complete information list should 
#' be returned
#' 
#' @return the name of the loading namespace if \code{env} and \code{info} are 
#' \code{FALSE}, an environment if \code{env=TRUE}, a list with elements 
#' \code{pkgname} and \code{libname} if \code{info=TRUE}. 
#' 
#' @rdname devutils
#' 
getLoadingNamespace <- function(env=FALSE, info=FALSE){
	is.loading <- try(nsInfo <- loadingNamespaceInfo(), silent=TRUE)
	if( !is(is.loading, 'try-error') ){
		if( env ) asNamespace(as.name(nsInfo$pkgname))
		else if( info ) nsInfo 
		else nsInfo$pkgname
	}
	else NULL
}

#' \code{packageName} returns the current package's name.
#' 
#' @rdname devutils
#' @return a character string
packageName <- function(){
	# try to find the name from the package's environment (namespace) 
	if( exists('.packageName', packageEnv()) && .packageName != 'datasets' ){
		if( .packageName != '' )
			return(.packageName)
	}
	# get the info from the loadingNamespace
	info <- getLoadingNamespace(info=TRUE)
	if( !is.null(info) ) # check whether we are loading the namespace 
		info$pkgname
	else{# we are in dev mode: use devtools
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
	
	# try to find the path from the package's environment (namespace)
	if( exists('.packageName', packageEnv()) && .packageName != 'datasets' ){
		# get the path from installation
		path <- system.file(package=.packageName)
		# somehow this fails when loading an installed package but is works 
		# when loading a package during the post-install check
		if( path != '' ) return(path)
	}
	# get the info from the loadingNamespace
	info <- getLoadingNamespace(info=TRUE)
	if( !is.null(info) ) # check whether we are loading the namespace 
		file.path(info$libname, info$pkgname)
	else{# we are in dev mode: use devtools
		library(devtools)
		p <- as.package(.LOCAL_PKG_NAME)
		return(p$path)
	}
}

roctave <- function(end=TRUE, load=TRUE){
	
	if( load )
		load_all(.LOCAL_PKG_NAME)
	if( end ) 
		oend()
	
	# compile source files
	compile_src()
	
	#ostart()	
}