\name{OctaveFunction}
\alias{OctaveFunction}
\alias{OctaveFunction-class}
\title{Wrapper Class for Octave Functions}
\description{
  Wrapper Class for Octave Functions

  The function \code{OctaveFunction} is a
  constructor/factory method for \code{OctaveFunction}
  objects.
}
\usage{
  OctaveFunction(name)
}
\arguments{
  \item{name}{the name of an Octave function.}
}
\examples{
osvd <- OctaveFunction('svd')
osvd
osvd(matrix(1:9,3))

orand <- OctaveFunction('rand')
orand()
orand(2)
}
\keyword{internal}

