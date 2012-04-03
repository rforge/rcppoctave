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
		p <- devtools::as.package(pkg)
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
			devtools::load_c(pkg)
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
packageEnv <- function() topenv()

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
		p <- devtools::as.package(.LOCAL_PKG_NAME)
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
		p <- devtools::as.package(.LOCAL_PKG_NAME)
		return(p$path)
	}
}

roctave <- function(end=TRUE, load=TRUE){
	
	if( load )
		devtools::load_all(.LOCAL_PKG_NAME)
	if( end ) 
		oend()
	
	# compile source files
	compile_src()
	
	#ostart()	
}

makeFakeVignette <- function(template, out){
	l <- readLines(template)
	cat(c("\\documentclass[10pt]{article}"
		, l[grep("^%\\\\Vignette", l)]
		, "\\usepackage{url}\n\\usepackage[colorlinks]{hyperref}\n\n\\begin{document}\n\\end{document}")
	, file=out, sep="\n");
}

#' Generate a Bibtex File from Package Citations
#' 
#' Generates a Bibtex file from a list of packages or all the installed packages.
#' It is useful for adding relevant citations in Sweave documents.
#' 
#' Multiple citations are handled by adding a numeric suffix to the Bibtex key 
#' (other than the first/main citation) as \code{"<pkgname>\%i"} (e.g. pkg, pkg2, pkg3).
#' 
#' This function has now been integrated by Romain François in the bibtex package.
#'
#' @encoding utf8
#'
#' @param entry a \code{\link{bibentry}} object or a character vector of package 
#' names. If \code{NULL}, then the list of all installed packages is used.
#' @param file output Bibtex file. It can be specified as a filename (as a single 
#' character string), NULL for \code{stdout}, or a \code{link{connection}} object. 
#' If \code{file} is a character string, an extension '.bib' is appended if not 
#' already present.
#' @param append a logical that indicates that the Bibtex entries should be added
#' to the file. If \code{FALSE} (default), the file is overwritten.  
#' @param verbose a logical to toggle verbosity. If \code{file=NULL}, verbosity 
#' is forced off. 
#'
#' @return the list of Bibtex objects -- invisibly.
#' @author
#' Renaud Gaujoux, based on the function \code{Rpackages.bib} 
#' from Achim Zeileis (see \emph{References}).
#' 
#' @references 
#' \emph{[R] Creating bibtex file of all installed packages?}
#' Achim Zeileis. R-help mailing list. 
#' \url{https://stat.ethz.ch/pipermail/r-help/2009-December/222201.html}
#' 
#' @seealso \code{link{connection}}, \code{link{bibentry}}
#'  
#' @export
#' @examples
#' 
#' library(bibtex)
#' write.bib(c('bibtex', 'utils', 'tools'), file='references')
#' bibs <- read.bib('references.bib')
#' write.bib(bibs, 'references2.bib')
#' md5 <- tools::md5sum(c('references.bib', 'references2.bib'))
#' md5[1] == md5[2]
#' \dontshow{ stopifnot(md5[1] == md5[2]) }
#' 
#' # write to stdout()
#' write.bib(c('bibtex', 'utils', 'tools'), file=NULL)
#' 
write.bib <- function(entry=NULL, file="Rpackages.bib", append = FALSE, verbose = TRUE)
{
	# special handling of file=NULL: use stdout()
	if( is.null(file) ){
		file <- stdout()
		verbose <- FALSE
	}	
	## use all installed packages if nothing is specified
	if( is.null(entry) ){ 
		if( verbose ) message("Generating Bibtex entries for all installed packages ", appendLF=FALSE)
		entry <- unique(installed.packages()[,1])
		if( verbose ) message("[", length(entry), "]")
	}
	
	bibs <- 
			if( is(entry, 'bibentry') )	entry
			else if( is.character(entry) ){
				if( length(entry) == 0 ){
					if( verbose ) message("Empty package list: nothing to be done.")
					return(invisible())
				}
				
				pkgs <- entry
				bibs <- sapply(pkgs, function(x) try(citation(x)), simplify=FALSE)
				#bibs <- lapply(pkgs, function(x) try(toBibtex(citation(x))))
				n.installed <- length(bibs)
				
				## omit failed citation calls
				ok <- sapply(bibs, is, 'bibentry')
				pkgs <- pkgs[ok]
				bibs <- bibs[ok]
				n.converted <- sum(ok)
				
				## add bibtex keys to each entry
				pkgs <- lapply(seq_along(pkgs), function(i) if(length(bibs[[i]]) > 1)
								paste(pkgs[i], c('', 2:length(bibs[[i]])), sep = "") else pkgs[i])
				pkgs <- do.call("c", pkgs)
				bibs <- do.call("c", bibs)		
				# formatting function for bibtex keys:
				# names with special characters must be enclosed in {}, others not.
				as.bibkey <- function(x){
					i <- grep("[.]", x)
					if( length(i) > 0 )
						x[i] <- paste("{", x[i], "}", sep='')
					x
				}		
				#bibs <- mapply(function(b,k){ if( is.null(b$key) ) b$key <- as.bibkey(k); b}, bibs, pkgs, SIMPLIFY=FALSE)
				bibs <- mapply(function(b,k){ if( is.null(b$key) ) b$key <- k; b}, bibs, pkgs, SIMPLIFY=FALSE)
				bibs <- do.call("c", bibs)
				
				if(verbose) message("Converted ", n.converted, " of ", n.installed, " package citations to BibTeX")					
				bibs
			} else
				stop("Invalid argument `entry`: expected a bibentry object or a character vector of package names.")
	
	if( length(bibs) == 0 ){
		if( verbose ) message("Empty bibentry list: nothing to be done.")
		return(invisible())
	}
	
	## write everything to the .bib file
	fh <- if( is.character(file) ){
				if( !grepl("\\.bib$", file) ) # add .bib extension if necessary 
					file <- paste(file, '.bib', sep='')
				fh <- file(file, open = if(append) "a+" else "w+" )
				on.exit( if( isOpen(fh) ) close(fh) )
				fh
			} else if( is(file, 'connection') )
				file
			else
				stop("Invalid argument `file`: expected a filename, NULL, or a connection [", class(file), "]")
	
	if( !is(fh, 'connection') )
		stop("Invalid connection: ", fh)		
	file.desc <- summary(fh)['description']
	
	if( verbose ) message(if( append ) "Adding " else "Writing ", length(bibs) , " Bibtex entries ... ", appendLF=FALSE)
	writeLines(toBibtex(bibs), fh)
	if(verbose) message("OK\nResults written to file '", file.desc, "'")
	
	## return Bibtex items invisibly
	invisible(bibs)
}
