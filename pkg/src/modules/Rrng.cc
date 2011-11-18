// Octave modules for R Random Number Generator
// Copyright (C) 2011 Renaud Gaujoux <renaud@cbio.uct.ac.za>
//
// The code for the RNGs was extracted from R-2.14.0
// http://cran.r-project.org
//
// This code was initially inspired from Dirk Eddelbuettel's randmt Octave module.
// http://dirk.eddelbuettel.com/code/octave-mt.html
// Copyright (C) 1998, 1999 Dirk Eddelbuettel <edd@debian.org>
//
// This file provides the following Octave functions:
//    runif		for Uniform random number
//    rnorm		for Normal random number
//    rgamma	for Gamma random number
//    setseed	to set the seed of the current RNG
//
//     
//	This program is free software: you can redistribute it and/or modify
//	it under the terms of the GNU General Public License as published by
//	the Free Software Foundation, either version 3 of the License, or
//	(at your option) any later version.
//
//	This program is distributed in the hope that it will be useful,
//	but WITHOUT ANY WARRANTY; without even the implied warranty of
//	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//	GNU General Public License for more details.
//
//	You should have received a copy of the GNU General Public License
//	along with this program.  If not, see <http://www.gnu.org/licenses/>.

// Octave includes
#include <octave/config.h>
#include <octave/oct-obj.h>
#include <octave/oct-map.h>
#include <octave/defun-dld.h>

// STD includes
#include <limits.h>

#include "libRrng.h"

#define qnorm qnorm5

static void init(){
	static bool initRNG = true;
	if( initRNG ){
		printf("Init RNG Seed\n");
		do_setseed(0, NULL, NULL);
		initRNG = false;
	}
}

DEFUN_DLD (setseed, args, nargout,
 "setseed(n)\n\n\
Sets the seed for the current RNG.\n")
{
  init();
  octave_value_list retval;	// list of return values

  int nargs = args.length ();	// number of arguments supplied

  if (nargs != 1) {		// if wrong arguments, show message
    usage("Wrong number of argument: expecting a single integer value.\nTry 'help setseed' for info");
    return retval;		// and return empty
  }

  long n = long(args(0).double_value());
  do_setseed((Int32) n, NULL, NULL);

  return retval;
}

DEFUN_DLD (getseed, args, nargout,
 "s = getseed()\n\n\
Returns the seed for the current RNG.\n\
The value correspond to the value of .Random.seed in R.\n\
See ?.Random.seed in an R session.")
{

  init();
  octave_value_list retval;	// list of return values

  int nargs = args.length ();	// number of arguments supplied

  if (nargs != 0) {		// if wrong arguments, show message
    usage("Wrong number of argument: expecting no arguments.\nTry 'help getseed' for info");
    return retval;		// and return empty
  }

  // initialize maximum
  int rs[MAX_SEED_LENGTH+1];
  int len = do_getseed(rs);
  int32NDArray m(dim_vector(1, len+1));
  for(int i=0; i<len+1; i++)
  	  m(0,i) = rs[i];

  retval(0) = m;
  return retval;
}

#define RAND_ARGS(octave_fun, max_arg) \
init(); \
octave_value_list retval;\
int nargs = args.length ();	\
\
if (nargs<1 || nargs>max_arg) { \
usage("Try 'help "octave_fun"' for info");\
return retval;\
}\
\
long n,k;\
octave_value tmp = args(0);\
if( tmp.is_matrix_type() ){\
Array<int> iv = tmp.int_vector_value (true);\
n = iv(0);\
k = iv(1);\
}else{\
n = long(args(0).double_value());\
k = ( nargs >= 2 && !args(1).is_empty() ? long(args(1).double_value()) : n);\
}\
\
Matrix X(n, k);

#define RAND_RESULT(octave_fun_call) \
for (long j=0; j<k; j++){\
	for (long i=0; i<n; i++){\
	  X(i,j) = octave_fun_call;\
	}\
}\
\
retval(0) = X;\
\
return retval;

#define RAND_FUNCTION(fun_rand, octave_fun) \
RAND_ARGS(octave_fun, 2) \
RAND_RESULT(fun_rand())

DEFUN_DLD (runif, args, nargout,
"USAGE: U = runif( n [, k])\n\n\
Generates uniform random variates using the current RNG.\n\
runif(n, k)   returns a n*k matrix with uncorrelated U(0, 1) deviates drawn in columns\n\
runif(n)      returns a n*n matrix with uncorrelated U(0, 1) deviates drawn in columns\n")
{
	 RAND_FUNCTION(unif_rand, "runif")
}

DEFUN_DLD (rnorm, args, nargout, 
 "USAGE: N = rnorm( n [, k])\n\n\
Generates standard-normal random variates using the current RNG.\n\
rnorm(n, k)   returns a n*k matrix with uncorrelated N(0, 1) deviates drawn in columns\n\
rnorm(n)      returns a n*n matrix with uncorrelated N(0, 1) deviates draw in columns\n")
{
	RAND_FUNCTION(norm_rand, "rnorm")
}

DEFUN_DLD (rexp, args, nargout, 
 "USAGE: E = rexp( n [, k])\n\n\
Generates standard-exponential random variates using the current RNG.\n\
rexp(n, k)   returns n*k matrix with uncorrelated E(0, 1) deviates drawn in columns\n\
rexp(n)      returns n*n matrix with uncorrelated E(0, 1) deviates drawn in columns\n")
{
	RAND_FUNCTION(exp_rand, "rexp")
}

DEFUN_DLD (rgamma, args, ,
"USAGE: E = rgamma( n [, k, shape, scale])\n\n\
Generates Gamma random variates using the current RNG.\n\n\
rgamma(n, k)   returns n*k matrix with uncorrelated E(0, 1) deviates drawn in columns\n\
rgamma(n)      returns n*n matrix with uncorrelated E(0, 1) deviates drawn in columns\n\
\nOptional parameters:\n\n\
a = mean of the standard gamma distribution.\n\
scale = scale of the standard gamma distribution.\n\
\nREFERENCES\n\n\
[1] Shape parameter a >= 1.  Algorithm GD in:\n\n\
Ahrens, J.H. and Dieter, U. (1982).\n\
Generating gamma variates by a modified rejection technique.\n\
Comm. ACM, 25, 47-54.\n\n\
\n\
[2] Shape parameter 0 < a < 1. Algorithm GS in:\n\n\
Ahrens, J.H. and Dieter, U. (1974).\n\
Computer methods for sampling from gamma, beta, poisson and binomial distributions.\n\
Computing, 12, 223-246.\n\n\n\
LICENSE INFORMATION\n\n\
GNU General Public License (>=2)\n\n\
Mathlib : A C Library of Special Functions\n\
Copyright (C) 1998 Ross Ihaka\n\
Copyright (C) 2000--2008 The R Development Core Team\n\
Copyright (C) 2011 Renaud Gaujoux (Standalone library + Octave module)")
{
	
  RAND_ARGS("rgamma", 4)

  int nArgs = args.length ();	// number of arguments supplied

  // retrieve shape and scale parameters
  double shape(nArgs >= 3 && !args(2).is_empty() ? args(2).double_value() : 1);
  double scale(nArgs >= 4 && !args(3).is_empty() ? args(3).double_value() : 1);

  RAND_RESULT(rgamma(shape, scale))\
}

