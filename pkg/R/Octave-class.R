# Class Octave: direct interface to Octave functions and base context
# 
# Author: Renaud Gaujoux
# Creation: 05 Nov 2011
###############################################################################


#' Class Octave: Seamless Access to Octave Functions and Variables
#' 
#' This class provides a direct interface to Octave functions and base context
#' (i.e. the scope where user objects are defined).
#'  
#' 
#' @keywords internal
setClass("Octave", contains='character')

#' Direct Interface to Octave 
#' 
#' \code{RcppOctave} provides a simple interface to Octave via the  
#' object \code{.O}, an instance of class \code{Octave}, that allows for direct access
#' to Octave functions and variables using calls such as: \code{.O$svd(matrix(1:9,3))}.
#' 
#' 
#' @rdname OctaveInterface
#' @format \code{.O} is an object of class \code{\linkS4class{Octave}}.
#' @export
#' @examples
#' .O
#' .O$help()
#' .O$a <- 10
#' .O$a
#' 
.O <- new("Octave")

#' @rdname OctaveInterface
#' @export
setMethod('show', 'Octave',
		function(object){
			cat("<Octave Interface>\n")
			cat("Use `$<name>` to call Octave function or get variable <name>.\n")
			cat("Use `$<name>` <- to set the value of the Octave variable <name>.\n")
		}
)

#' @importMethodsFrom methods show
setGeneric('show', package='methods')

setGeneric('.DollarNames', package='utils')

o_completion_matches <- function(pattern = ""){
	# remove leading "^"	
	opattern <- sub("^[\\^]?(.*)", "\\1", pattern)
	grep(pattern, .CallOctave('completion_matches', opattern), value=TRUE)	
}

o_exist <- function(NAME, ...){
	.CallOctave('exist', NAME, ...)
}

.DollarNames.Octave <- function(x, pattern = "") o_completion_matches(pattern)

#' The method \code{$} provides a direct way of calling Octave functions or 
#' retrieving variables from Octave base context, via e.g. \code{.O$svd(x)} 
#' or \code{.O$a}.
#'  
#' @rdname OctaveInterface
#' @export
setMethod('.DollarNames', 'Octave', .DollarNames.Octave)

#' @rdname OctaveInterface
#' @export
setMethod('$', 'Octave', function(x, name)	o_get(name))

#' The method \code{$<-} allow to directly assign/set Octave variables via e.g.
#' \code{.O$a <- 10}. 
#' 
#' @rdname OctaveInterface
#' @export 
setReplaceMethod('$', 'Octave',
	function(x, name, value){
		value <- eval(value)
		o_assign(name, value)
		x
	}
)

