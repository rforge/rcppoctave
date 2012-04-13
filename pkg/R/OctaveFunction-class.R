# Wrapper class for Octave functions
# 
# Author: Renaud Gaujoux
# Creation: 05 Nov 2011
###############################################################################

#' Wrapper Class for Octave Functions
#' 
#' @slot name name of the wrapped Octave function
#' 
setClass("OctaveFunction", contains="function"
	, representation(name='character')	
)

#' The function \code{OctaveFunction} is a constructor/factory method for 
#' \code{OctaveFunction} objects.
#' 
#' @param name the name of an Octave function.
#' 
#' @keywords internal
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
#' 
OctaveFunction <- function(name){
	
	e <- new.env()
	f <- evalq({
		.NAME <- name
		function(...){
			.CallOctave(.NAME, ...)		
		}		
	}, e)
	new('OctaveFunction', f, name=name)
}

#' @rdname OctaveFunction-class
#' @export
setMethod('show', 'OctaveFunction', function(object){
	cat("<OctaveFunction::`", object@name, "`>\n", sep='')	
})

