\docType{class}
\name{OctaveFunction-class}
\alias{OctaveFunction}
\alias{OctaveFunction-class}
\alias{show,OctaveFunction-method}
\title{Wrapper Class for Octave Functions}
\usage{
  OctaveFunction(name)

  \S4method{show}{OctaveFunction}(object)
}
\arguments{
  \item{name}{the name of an Octave function.}

  \item{object}{Any R object}
}
\description{
  Wrapper Class for Octave Functions

  The function \code{OctaveFunction} is a
  constructor/factory method for \code{OctaveFunction}
  objects.
}
\section{Slots}{
  \describe{

  \item{name}{name of the wrapped Octave function}

  }
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
\keyword{methods}

