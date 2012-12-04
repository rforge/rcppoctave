# Wrapper class for Octave functions
# 
# Author: Renaud Gaujoux
# Creation: 05 Nov 2011
###############################################################################

#' Wrapping and Defining Octave Functions from R
#' 
#' @slot name name of the wrapped Octave function
#' 
setClass("OctaveFunction", contains="function"
	, representation(name='character')	
)

#' The function \code{OctaveFunction} is a constructor/factory method for 
#' \code{OctaveFunction} objects, which wrap calls to Octave functions into 
#' plain R functions.
#' 
#' @param name the name or definition of an Octave function.
#'
#' @rdname OctaveFunction-class
#' @export
#' @examples
#' 
#' osvd <- OctaveFunction('svd')
#' osvd
#' osvd(matrix(1:9,3))
#' 
#' orand <- OctaveFunction('rand')
#' orand()
#' orand(2)
#' orand(2, 3)
#' 
#' # From source code
#' myfun <- OctaveFunction('function [Y] = somefun(x)
#' 	Y = x * x;
#' 	end
#' ')
#' myfun
#' myfun(10)
#' 
OctaveFunction <- function(name){
	
	e <- new.env()
	# extract function name and save code in temporary file 
	# if necessary
	if( any(d <- is_mdef(name)) ){
		o_source(text=name)
		name <- names(d)[which(d)]
	}
	
	f <- evalq({
		.NAME <- name
		function(...){
			.CallOctave(.NAME, ...)		
		}		
	}, e)
	new('OctaveFunction', f, name=name)
}


is_mdef <- function(x){
	m <- str_match(x, "((^)|([\n;]))\\s*function\\s*(\\[[^]*]\\])?\\s*=\\s*([^(]+)")
	setNames(!is.na(m[,1]), m[,6])
}

#' @rdname OctaveFunction-class
#' @export
setMethod('show', 'OctaveFunction', function(object){
	cat("<OctaveFunction::`", object@name, "`>\n", sep='')	
})

#' M Files
#' 
#' \code{mfiles} converts source code or .m filenames into real paths to .m files
#' that can be sourced with \code{\link[RcppOctave]{o_source}}.
#' 
#' @param ... specification of a .m files as character arguments.
#' The elements of the vector can be either file paths or plain Octave/Matlab code, 
#' which are then written to disk in -- temporary -- .m files. 
#' Note that the paths do not need to correspond to existing files.
#' @inheritParams base::tempfile
#' @param dir existing directory where to write the .m files generated from 
#' the plain code elements of \var{x}.
#' 
#' @export
#' @examples 
#' 
#' cat('', file='test.m')
#' f <- mfiles('test.m')
#' f <- mfiles('test.m', '')
#' # remove all files
#' unlink(f)
#' 
mfiles <- function(..., pattern='mfile_', dir=tempdir()){
	
	in_package <- FALSE
	if( missing(dir) && !is.null(ns <- getLoadingNamespace()) ){
		in_package <- TRUE
		dir <- packagePath('matlab', package=ns)
	}
	
	# get args
	x <- unlist(list(...))
	if( !is.character(x) )
		stop("All arguments must be character strings")
	
	# detect type of input
	isfile <- !is_mdef(x)
	# add names if needed
	if( is.null(names(x)) ) names(x) <- rep('', length(x))
	
	code <- x[!isfile]
	if( length(code) ){
		x[!isfile] <- mapply(function(f, x){
					
			# create directory if it does not exist
			if( !file.exists(dir) )	dir.create(dir, recursive=TRUE)
			
			# build file path
			ofile <- f
			if( nchar(f) ) f <- file.path(dir, f)
			else f <- tempfile(pattern, tmpdir=dir)
			f <- str_c(f, '.m')
			# write file
			cat(x, file=f)
			
			# return filepath
			if( in_package ) ofile else f
		}, names(code), code)
	}
	x
}
