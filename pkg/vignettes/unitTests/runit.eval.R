# Unit tests for eval functions: assign, get, etc...
# 
# Author: Renaud Gaujoux
# Creation: 17 Nov 2011
###############################################################################



#' Unit test for o_get
test.o_get <- function(){
	
	o_clear()
	o_eval('who')
	
	checkIdentical(o_get(), list(), "No argument + empty context: returns empty list")
	checkIdentical(o_get(ans=TRUE), list(ans=o_eval('ans')), "No argument + empty context + ans=FALSE: returns 'ans' value in a list")
	checkIdentical(o_get('ans'), o_eval('ans'), "o_get('ans'): returns 'ans' value")
	checkIdentical(o_get('ans', ans=FALSE), list(), "o_get('ans', ans=TRUE): returns empty list")
	
	o_clear()
	o_eval('who')
	l <- list(a=1)
	o_load(l)
	checkIdentical(o_get(), l, "No argument + single variable in context: returns single variable in list")
	checkIdentical(o_get(unlist=TRUE), l$a, "No argument + single variable in context + unlist: returns single variable value")
	checkIdentical(o_get('a'), l$a, "Single quoted argument + single variable in context: returns single variable value")
	checkIdentical(o_get('a', unlist=FALSE), l, "Single unquoted argument + single variable in context + unlist: returns single variable in list")
	
	o_clear()
	o_eval('who')
	l <- list(b=1, c=3, d=matrix(1:9, 3))
	o_load(l)
	
	checkIdentical(o_get(), l, "No argument + multiple variable in context: returns all variables in list")
	checkIdentical(o_get(ans=TRUE), c(list(ans=o_get('ans')), l), "No argument + ans=FALSE: returns all variables in list including 'ans'")
	checkIdentical(o_get('b'), l$b, "Single quoted argument: returns single variable value")
	checkIdentical(o_get('b', unlist=FALSE), l['b'], "Single unquoted argument: returns single variable value in list")
		
	checkIdentical(o_get(X='b'), list(X=l$b), "Single named argument: returns renamed variable in list")
	checkIdentical(o_get(X='b', unlist=TRUE), l$b, "Single named argument: returns variable value")
	checkIdentical(o_get(X='b', Y='c'), list(X=l$b, Y=l$c), "Two named arguments: returns renamed variables")
	checkIdentical(o_get(X='b', 'c', 'd'), list(X=l$b, c=l$c, d=l$d), "Mix of named/unnamed arguments: returns renamed/named variable list")
	
	f <- o_get('svd')
	checkTrue( is(f, 'OctaveFunction'), "Get of a function returns an OctaveFunction object")
	checkIdentical( f@name, 'svd', "Get of a function returns the correct OctaveFunction object")
	
	f <- o_get(a='svd')
	checkTrue( is(f$a, 'OctaveFunction'), "Get of a function with named argument returns a list with the OctaveFunction object in named element")
	checkIdentical( f$a@name, 'svd', "Get of a function with named argument returns the correct OctaveFunction object in list")
	
}
