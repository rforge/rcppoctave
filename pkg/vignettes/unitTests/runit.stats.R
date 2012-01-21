# Unit tests for stat functions.
#
# Copyright (C) 2011 Renaud Gaujoux
# 
# This file is part of RcppOctave.
#
# RcppOctave is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# RcppOctave is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with RcppOctave.  If not, see <http://www.gnu.org/licenses/>.

#' Unit test for o_setseed
test.o_set.seed <- function(){
	
	set.seed(123)
	o_set.seed(123)
	#message("\nR seed: ", paste(head(.Random.seed, 20), collapse=', '))
	#message("\nOctave seed: ", paste(head(o_Random.seed(), 20), collapse=', '))
	checkIdentical( .Random.seed, o_Random.seed(), ".Random.seed are identical after using respective set.seed()")	
}


# Auxiliary function for testing the o_r* functions
check.rfun <- function(rfun, ofun, ..., .msg=NULL){
	
	rname <- deparse(substitute(rfun))
	oname <- deparse(substitute(ofun))
	title <- paste(rname, ' and ', oname, if( !missing(.msg) ) paste('-', .msg),  ':', sep='')
	msg <- function(...) paste(title, ...)
	set.seed(123)
	o_set.seed(123)
	checkIdentical( .Random.seed, o_Random.seed(),  msg('.Random.seed are identical before initial draws'))
	checkIdentical( rfun(10, ...), ofun(1,10, ...), msg('random vector draws are identical'))
	checkIdentical( .Random.seed, o_Random.seed(),  msg('.Random.seed are identical after vector draw'))
	checkIdentical( matrix(rfun(25, ...), 5, 5), ofun(5, ...), msg('random matrix draws are identical'))
	checkIdentical( .Random.seed, o_Random.seed(),  msg('.Random.seed are identical after matrix draw'))
}

#' Common unit test for o_runif
test.o_runif <- function(){	
	check.rfun(runif, o_runif)
}

#' Unit test for o_rnorm
test.o_rnorm <- function(){	
	check.rfun(rnorm, o_rnorm)
}

#' Unit test for o_rexp
test.o_rexp <- function(){	
	check.rfun(rexp, o_rexp)
}

#' Unit test for o_rgamma
test.o_rgamma <- function(){	
	check.rfun(rgamma, o_rgamma, shape=1, scale=1, .msg='shape=1 / scale=1')
	check.rfun(rgamma, o_rgamma, shape=10, scale=10, .msg='shape=10 / scale=10')
	check.rfun(rgamma, o_rgamma, shape=10, scale=1, .msg='shape=10 / scale=1')
}