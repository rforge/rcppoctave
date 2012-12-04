\docType{class}
\name{OctaveFunction-class}
\alias{OctaveFunction}
\alias{OctaveFunction-class}
\alias{show,OctaveFunction-method}
\title{Wrapping and Defining Octave Functions from R}
\usage{
  OctaveFunction(name)

  \S4method{show}{OctaveFunction}(object)
}
\arguments{
  \item{name}{the name or definition of an Octave
  function.}

  \item{object}{Any R object}
}
\description{
  Wrapping and Defining Octave Functions from R

  The function \code{OctaveFunction} is a
  constructor/factory method for \code{OctaveFunction}
  objects, which wrap calls to Octave functions into plain
  R functions.
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
orand(2, 3)

# From source code
myfun <- OctaveFunction('function [Y] = somefun(x)
	Y = x * x;
	end
')
myfun
myfun(10)
}
\keyword{methods}

