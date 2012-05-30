# Run all unit tests in installed directory unitTests
# 
# Author: Renaud Gaujoux
# Creation: 26 Oct 2011
###############################################################################


#### doRUnit.R --- Run RUnit tests
####------------------------------------------------------------------------

### borrowed from package Rcpp
### http://cran.r-project.org/web/packages/Rcpp/index.html
###
### Adapted to the package RcppOctave
if(require("RUnit", quietly = TRUE)) {
	pkg <- "RcppOctave"
	
	require( pkg, character.only=TRUE)
	
	path <- system.file("unitTests", package = pkg)
	
	stopifnot(file.exists(path), file.info(path.expand(path))$isdir)
	
	# without this, we get unit test failures
	Sys.setenv( R_TESTS = "" )
	
	RcppOctave.unit.test.output.dir <- getwd()
	
	source(file.path(path, "runTests.R"), echo = TRUE)
	
} else {
	print( "package RUnit not available, cannot run unit tests" )
}
