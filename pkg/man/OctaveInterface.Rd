\docType{data}
\name{.O}
\alias{.DollarNames,Octave-method}
\alias{.O}
\alias{$<-,Octave-method}
\alias{$,Octave-method}
\alias{show,OctaveFunction-method}
\alias{show,Octave-method}
\title{Direct Interface to Octave}
\format{\code{.O} is an object of class \code{\linkS4class{Octave}}.}
\usage{
  .O
}
\description{
  \code{RcppOctave} provides a simple interface to Octave
  via the object \code{.O}, an instance of class
  \code{Octave}, that allows for direct access to Octave
  functions and variables using calls such as:
  \code{.O$svd(matrix(1:9,3))}.
}
\section{Methods}{
  \describe{ \item{.DollarNames,Octave-method}{The method
  \code{$} provides a direct way of calling Octave
  functions or retrieving variables from Octave base
  context, via e.g. \code{.O$svd(x)} or \code{.O$a}. It is
  equivalent to \code{o_get(name, exact=TRUE)}, meaning
  that no partial match is performed and \code{name} must
  correspond exactly to a variable or a function name. See
  generic \code{\link{.DollarNames}}.

  Usage: .DollarNames(NA) }

  \item{$<-,Octave-method}{The method \code{$<-} allow to
  directly assign/set Octave variables via e.g. \code{.O$a
  <- 10}. See generic \code{\link{$<-}}.

  Usage: $<-(x, name, value) }

  }
}
\examples{
.O
# assign/get Octave variables
.O$a <- 10
.O$a

# call Octave functions
.O$help()
.O$svd(matrix(runif(9), 3))
}
\seealso{
  \code{\link{o_get}}
}
\keyword{datasets}

