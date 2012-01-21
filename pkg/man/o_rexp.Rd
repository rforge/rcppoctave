\name{o_rexp}
\alias{o_rexp}
\title{Drawing from R Exponential Distribution in Octave}
\usage{
  o_rexp(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\description{
  Drawing from R Exponential Distribution in Octave
}
\section{Octave Documentation for \emph{rexp}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rexp,rd=TRUE)}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random exponential values (in vector form)
o_rexp(1)
o_rexp(1, 10)

# Draw random normal values (in matrix form)
o_rexp(2)
o_rexp(2, 5)
}
\seealso{
  rexp

  Other orandom: \code{\link{o_Random.seed}},
  \code{\link{o_rgamma}}, \code{\link{o_rnorm}},
  \code{\link{o_runif}}, \code{\link{o_set.seed}}
}

