\name{o_rgamma}
\alias{o_rgamma}
\title{Drawing from R Gamma Distribution in Octave}
\description{
  Drawing from R Gamma Distribution in Octave
}
\usage{
  o_rgamma(n, p = n, shape = 1, scale = 1)
}
\arguments{
  \item{shape}{Mean of the Gamma distribution}

  \item{scale}{Scale of the Gamma distribution}

  \item{n}{number of output rows}

  \item{p}{number of output columns (default to \code{n})}
}
\section{Octave Documentation for \emph{rgamma}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(rgamma,rd=TRUE)}
}
\examples{
# Draw random gamma values (in vector form)
o_rgamma(1)
o_rgamma(1, 10)

# Draw random gamma values (in matrix form)
o_rgamma(2)
o_rgamma(2, 5)

# Draw random gamma values with shape and scale parameters
o_rgamma(1, 5, shape=2)
o_rgamma(1, 10, scale=0.5)
}
\seealso{
  rgamma

  Other orandom: \code{\link{o_Random.seed}},
  \code{\link{o_rexp}}, \code{\link{o_rnorm}},
  \code{\link{o_runif}}, \code{\link{o_set.seed}}
}

