\name{o_rnorm}
\alias{o_rnorm}
\title{Drawing from R Normal Distribution in Octave}
\usage{
  o_rnorm(n, p = n)
}
\arguments{
  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\description{
  Drawing from R Normal Distribution in Octave
}
\section{Octave Documentation for \emph{rnorm}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rnorm,rd=TRUE)}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\examples{
# Draw random normal values (in vector form)
o_rnorm(1)
o_rnorm(1, 10)

# Draw random normal values (in matrix form)
o_rnorm(2)
o_rnorm(2, 5)
}
\seealso{
  rnorm

  Other orandom: \code{\link{o_Random.seed}},
  \code{\link{o_rexp}}, \code{\link{o_rgamma}},
  \code{\link{o_runif}}, \code{\link{o_set.seed}}
}

