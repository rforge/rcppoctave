\name{OctaveConfig}
\alias{OctaveConfig}
\title{Cached RcppOctave Configuration Paths}
\description{
  Cached RcppOctave Configuration Paths
}
\usage{
  OctaveConfig(name, ...)
}
\arguments{
  \item{name}{Name of an RcppOctave path variable}

  \item{...}{extra names to be concatenated to the result
  with \code{\link{file.path}}. Only used when \code{name}
  is not missing.}
}
\value{
  a list (if \code{name is missing}) or a single character
  string.
}
\examples{
OctaveConfig()
OctaveConfig('lib')
OctaveConfig('include')
}
\keyword{internal}

