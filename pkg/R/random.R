# Function related to Random Number Generators
# 
# Author: Renaud Gaujoux
# Creation: 04 Nov 2011
###############################################################################


#' Drawing from R Uniform Distribution in Octave
#' 
#' This function calls the Octave port of the R functions \code{\link{runif}}-like 
#' functions.
#' 
#' @param n number of output rows
#' @param p number of output columns (default to \code{n})
#' 
#' @return a numeric vector or a matrix
#' 
#' @templateVar name runif
#' @template OctaveDoc
#' 
#' @family orandom 
#' @export
#' @seealso runif
#' @examples
#' 
#' # Draw random uniform values (in vector form)
#' o_runif(1)
#' o_runif(1, 10)
#' 
#' # Draw random uniform values (in matrix form)
#' o_runif(2)
#' o_runif(2, 5)
#' 
o_runif <- function(n, p=n){
	.CallOctave('runif', n, p)	
}

#' Drawing from R Normal Distribution in Octave
#' 
#' @inheritParams o_runif
#' 
#' @templateVar name rnorm
#' @template OctaveDoc
#' @export
#' @family orandom
#' @seealso rnorm
#' @examples
#' 
#' # Draw random normal values (in vector form)
#' o_rnorm(1)
#' o_rnorm(1, 10)
#' 
#' # Draw random normal values (in matrix form)
#' o_rnorm(2)
#' o_rnorm(2, 5)
#' 
o_rnorm <- function(n, p=n){
	.CallOctave('rnorm', n, p)
}

#' Drawing from R Exponential Distribution in Octave
#' 
#' @inheritParams o_runif 
#' 
#' @templateVar name rexp
#' @template OctaveDoc
#' @export
#' @family orandom
#' @seealso rexp
#' @examples
#' 
#' # Draw random exponential values (in vector form)
#' o_rexp(1)
#' o_rexp(1, 10)
#' 
#' # Draw random normal values (in matrix form)
#' o_rexp(2)
#' o_rexp(2, 5)
#' 
o_rexp <- function(n, p=n){
	.CallOctave('rexp', n, p)
}

#' Drawing from R Gamma Distribution in Octave
#' 
#' @inheritParams o_runif 
#' @param shape Mean of the Gamma distribution
#' @param scale Scale of the Gamma distribution
#' 
#' @templateVar name rgamma
#' @template OctaveDoc
#' 
#' @export
#' @family orandom
#' @seealso rgamma 
#' @examples
#' 
#' # Draw random gamma values (in vector form)
#' o_rgamma(1)
#' o_rgamma(1, 10)
#' 
#' # Draw random gamma values (in matrix form)
#' o_rgamma(2)
#' o_rgamma(2, 5)
#' 
#' # Draw random gamma values with shape and scale parameters
#' o_rgamma(1, 5, shape=2)
#' o_rgamma(1, 10, scale=0.5)
#' 
o_rgamma <- function(n, p=n, shape=1, scale=1){
	.CallOctave('rgamma', n, p, shape, scale)
}

#' Seed R RNG in Octave
#' 
#' This function seeds the Octave port of R random number generators.
#' 
#' The RNGs are provided by the Octave module \code{Rrng.oct}.
#' This module is built against the \code{libRrng} library, that provides R RNG 
#' original implementation, extracted from R-2.14.0. 
#' 
#' @param seed a single integer seed.
#' @return None
#' 
#' @export
#' @seealso set.seed
#' @family orandom
#' @examples
#' 
#' o_set.seed(12345)
#' o_runif(5)
#' o_runif(5)
#' 
#' o_set.seed(12345)
#' o_runif(5)
#' 
o_set.seed <- function(seed){
	.CallOctave('setseed', seed)
	invisible()
}

#' Get RNG in Octave
#' 
#' @return None
#' 
#' @export
#' @rdname o_set.seed
#' @examples
#' 
#' # Set RNG seed in R
#' set.seed(12345)
#' seedR <- .Random.seed
#' 
#' # Set RNG seed in Octave
#' o_set.seed(12345)
#' seedO <- o_Random.seed()
#' 
#' identical(seedR, seedO)
#' \dontshow{ stopifnot( identical(seedR, seedO) ) }
#' 
#' identical( runif(10), o_runif(1, 10) )
#' \dontshow{ stopifnot( identical( runif(10), o_runif(1, 10) ) ) }
#'  
o_Random.seed <- function(){
	.CallOctave('getseed')	
}


