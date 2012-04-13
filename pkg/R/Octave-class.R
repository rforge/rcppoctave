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
#' # assign/get Octave variables
#' .O$a <- 10
#' .O$a
#' 
#' # call Octave functions
#' .O$help()
#' .O$svd(matrix(runif(9), 3))
#' 
.O <- new("Octave")

#' @rdname OctaveInterface
#' @export
setMethod('show', 'Octave',
	function(object){
		cat(" <Octave Interface>\n")
		cat(" - Use `$x` to call Octave function or get variable x.\n")
		cat(" - Use `$x <- val` to assign a value val to the Octave variable x.\n")
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


#' @noRd
#' @S3method .DollarNames Octave
#' @export
.DollarNames.Octave <- function(x, pattern = "") o_completion_matches(pattern)

#' The method \code{$} provides a direct way of calling Octave functions or 
#' retrieving variables from Octave base context, via e.g. \code{.O$svd(x)} 
#' or \code{.O$a}.
#' It is equivalent to \code{o_get(name, exact=TRUE)}, meaning that no partial 
#' match is performed and \code{name} must correspond exactly to a variable or 
#' a function name. 
#'  
#' @rdname OctaveInterface
#' @seealso \code{\link{o_get}}
#' @export
setMethod('.DollarNames', 'Octave', .DollarNames.Octave)

#' @rdname OctaveInterface
#' @export
setMethod('$', 'Octave', function(x, name)	o_get(name, exact=TRUE))

#' The method \code{$<-} allow to directly assign/set Octave variables via e.g.
#' \code{.O$a <- 10}. 
#' 
#' @rdname OctaveInterface
#' @export 
setReplaceMethod('$', 'Octave',
	function(x, name, value){
		# remove variable if value is directly NULL
		if( is.null(value) ){
			o_clear(name)			
		}else{
			# force evaluation now
			value <- force(value)
			# assign result of evaluation
			o_assign(name, value)
		}
		x
	}
)

