# Base wrapper functions to some Octave functions
# 
# Author: "Renaud Gaujoux"
# Creation: 27 Oct 2011
###############################################################################

#' Sourcing Octave/Matlab Files
#' 
#' This function sources an Octave file within the current Octave session.
#' The loaded functions are accessible by subsequent calls of 
#' \code{\link{.CallOctave}}.
#' 
#' @param file the path to the Octave/Matlab source file -- typically with 
#' extension ".m".
#' 
#' @templateVar name source
#' @template OctaveDoc
#' 
#' @return None
#' @family Octave_files 
#'  
#' @export
#' 
o_source <- function(file){
	
	if( !file.exists(file) )
		stop("File `", file, "` does not exist.")
	
	.CallOctave('source', file)
	invisible()
}

#' Manipulating Octave Search Path 
#' 
#' Adds a directory at the beginning of Octave search path.
#' 
#' The .oct files present in directories from the search path are looked up 
#' when an object or function is requested but not loaded in the current session.
#' The files are watched and automatically reloaded in case modification.
#' 
#' @param DIR1 path specification to add to Octave search path. 
#' See section \emph{Octave Documentation}.
#' @param ... other path specifications
#' @param OPTION option that specifies how the path should be added. 
#' Possible values are: \code{'-begin', 0, '-end', 1}.
#' See section \emph{Octave Documentation}.
#' 
#' @return returns invisibly the old value of search path.
#' @family Octave_files
#' @export 
#' 
#' @templateVar name addpath
#' @template OctaveDoc
#' 
#' @examples
#' 
#' # call an undefined function
#' try(.CallOctave('fun1'))
#' 
#' # add to the path a directory with a .oct file that contains a definition for 'fun1'
#' o_addpath(system.file('scripts', package='RcppOctave'))
#' 
#' # re-call the function
#' #.CallOctave('fun1')
#' 
#' # change the .oct file
#' 
#' 
o_addpath <- function(DIR1, ..., OPTION=0L){
	invisible(.CallOctave('addpath', DIR1, ..., OPTION))
}

#' Get Octave Version
#' 
#' Returns the version of Octave currently used by \code{RcppOctave}.
#' 
#' @return Octave version as a single character string
#' 
#' @export
#' @examples
#' 
#' o_version()
#' 
o_version <- function(){
	.CallOctave('version')
}

#' Accessing Octave Help and Documentation Pages
#' 
#' \code{o_help} retrieves the Octave help page associated with a given symbol.
#' By default the page is printed out, but may also be silently retrieved or 
#' formatted for direct inclusion in R documentation files (i.e. Rd files). 
#' 
#' @param NAME Octave symbol (e.g. command, function, operator) passed to Octave 
#' function \code{help} to retrieve the related documentation.
#' @param character.only a logical indicating whether \code{NAME} can be
#' assumed to be a character string (\code{TRUE}) or should be substituted
#' with \code{\link{substitute}} before using them (default).
#' @param show logical that specifies if the help page should be shown using the 
#' as R documentation file (default), e.g. using a pager, or only returned as a 
#' single string. Note that when \code{show=TRUE}, the string is still returned 
#' but invisibly.
#' @param rd a logical that specifies if the result should be returned in a 
#' suitable way for including in Rd files. If \code{TRUE}, it wraps the Octave 
#' documentation string in Rd code that is rendered as in the Octave console.
#' 
#' @return this function is usually called for its side effect of printing the 
#' help page on standard output (argument \code{show=TRUE}), but it invisibly
#' returns the help page as a single character string.
#' 
#' @templateVar name help
#' @template OctaveDoc
#' 
#' @export
#' @examples 
#' 
#' o_help(print)
#' o_help(rand)
#' # or equivalently 
#' o_help('rand')
#' 
#' # to include in Rd files, use argument rd=TRUE in an \Sexpr:
#' # \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rand,rd=TRUE)}
#' 
#' # to see the included Rd code
#' o_help(rand, rd=TRUE)
#' 
o_help <- function(NAME, character.only = FALSE, show = TRUE, rd = FALSE){
	
	# substitute NAME
	if( !character.only )
		NAME <- as.character(substitute(NAME))
	
	# check argument length
	if( length(NAME) != 1 )
		stop("Argument `NAME` must be a single symbol or character string")
		
	# get the help page from Octave
	hlp <- .Call('oct_help', NAME)
	#print(hlp)
	
	if( rd ){
		# generate \Sexpr commands for each 
#		sexpr <- paste("RcppOctave::o_help(", NAME, ", show=FALSE)", sep='')
#		rdres <- paste(
#		"\\if{html}{\\Sexpr[results=rd,stage=render]{paste(\"\\\\\\\\out{<pre>\",", sexpr, ",'</pre>}', sep='')}}\n"
#		, "\\if{text}{\\Sexpr[results=rd,stage=render]{paste(\"\\\\\\\\out{\",", sexpr, ", '}', sep='')}}\n"
#		, "\\if{latex}{\\Sexpr[results=rd,stage=render]{paste(\"\\\\\\\\out{\\\\\\\\begin{verbatim}\",", sexpr, ", \"\\\\\\\\end{verbatim}}\", sep='')}}\n"
#		, sep='')
		rdres <- paste(
		"\\if{html}{\\out{<pre>", hlp, "</pre>}}\n"
		, "\\if{text}{\\out{", hlp, "}}\n"
		, "\\if{latex}{\\out{\\begin{verbatim}", hlp, "\\end{verbatim}}}\n"
		, sep='')
		return(rdres)
	}
		
	if( show ){
		hlpfile <- tempfile("OctaveDoc", fileext=".txt")
		title <- paste(NAME, "\t\t- Octave Documentation -\t\tv", o_version(), sep='')
		cat(paste(title, "\n\n", hlp, sep=""), file=hlpfile)
		file.show(hlpfile, title=, delete.file=TRUE)
		invisible(hlp)
	}else hlp
}

#' \code{o_doc} displays documentation for the function FUNCTION_NAME directly 
#' from an on-line version of the printed manual, using the GNU Info browser.
#' Type `q` to quit the browser.
#' 
#' @param FUNCTION_NAME the name of the function from which to show the 
#' documentation. See the relevant \emph{Octave Documentation} section below.
#' 
#' @templateVar name doc
#' @template OctaveDoc
#' 
#' @rdname o_help
#' @export
#' @examples
#' 
#' o_doc(text)
#' # or equivalently
#' o_doc('text')
#' 
o_doc <- function(FUNCTION_NAME){
	# substitute FUNCTION_NAME
	FUNCTION_NAME <- as.character(substitute(FUNCTION_NAME))
	# call Octave function `doc`
	invisible(.CallOctave('doc', FUNCTION_NAME))
}



#' Octave Identity Function
#' 
#' This function calls the Octave function provided by the module shipped with 
#' RcppOctave. It Returns its arguments unchanged, and is mainly used to test 
#' and check the effect of object conversions between R and Octave. 
#' 
#' @param ... any R object supported by RcppOctave.
#' @return its argument -- list -- after its conversion from R to Octave and 
#' from Octave to R.
#' 
#' @export
#' @examples
#' 
#' o_identity(1L)
#' o_identity(1:10)
#' o_identity(matrix(1:10, 2,5))
#' 
#' o_identity(1)
#' o_identity(runif(10))
#' o_identity(matrix(runif(10), 2,5))
#' 
o_identity <- function(...){
	
	dots <- list(...)
	if( length(dots) > 1 )
		sapply(dots, function(x) .CallOctave('identity', x), simplify=FALSE)
	else
		.CallOctave('identity', dots[[1]])
}

