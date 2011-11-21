\name{o_set.seed}
\alias{o_Random.seed}
\alias{o_set.seed}
\title{Seed R RNG in Octave}
\description{
  This function seeds the Octave port of R random number
  generators.

  Get RNG in Octave
}
\usage{
  o_set.seed(seed)

  o_Random.seed()
}
\arguments{
  \item{seed}{a single integer seed.}
}
\value{
  None

  None
}
\details{
  The RNGs are provided by the Octave module
  \code{Rrng.oct}. This module is built against the
  \code{libRrng} library, that provides R RNG original
  implementation, extracted from R-2.14.0.
}
\examples{
# Perform stochastic computations is R
set.seed(12345)
runif(5)
runif(5)
\dontshow{ rr <- runif(10) }

# Reproduce in Octave
o_set.seed(12345)
o_runif(1, 5)
o_runif(1, 5)
\dontshow{ stopifnot( identical(rr, o_runif(1, 10)) ) }

o_set.seed(12345)
o_runif(5)
# Set RNG seed in R
set.seed(12345)
seedR <- .Random.seed

# Set RNG seed in Octave
o_set.seed(12345)
seedO <- o_Random.seed()

identical(seedR, seedO)
\dontshow{ stopifnot( identical(seedR, seedO) ) }

identical( runif(10), o_runif(1, 10) )
\dontshow{ stopifnot( identical( runif(10), o_runif(1, 10) ) ) }
}
\seealso{
  set.seed

  Other orandom: \code{\link{o_rexp}},
  \code{\link{o_rgamma}}, \code{\link{o_rnorm}},
  \code{\link{o_runif}}
}

