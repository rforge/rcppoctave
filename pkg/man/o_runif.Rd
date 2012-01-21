\name{o_runif}
\alias{o_runif}
\title{Drawing from R Uniform Distribution in Octave}
\usage{
  o_runif(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\value{
  a numeric vector or a matrix
}
\description{
  This function calls the Octave port of the R functions
  \code{\link{runif}}-like functions.
}
\section{Octave Documentation for \emph{runif}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(runif,rd=TRUE)}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random uniform values (in vector form)
o_runif(1)
o_runif(1, 10)

# Draw random uniform values (in matrix form)
o_runif(2)
o_runif(2, 5)
}
\seealso{
  runif

  Other orandom: \code{\link{o_Random.seed}},
  \code{\link{o_rexp}}, \code{\link{o_rgamma}},
  \code{\link{o_rnorm}}, \code{\link{o_set.seed}}
}

