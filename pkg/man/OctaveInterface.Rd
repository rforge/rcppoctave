\docType{data}
\name{.O}
\alias{.O}
\alias{$<-,Octave-method}
\alias{$,Octave-method}
\title{Direct Interface to Octave}
\format{\code{.O} is an object of class \code{\linkS4class{Octave}}.}
\usage{
  .O

  \S4method{$}{Octave}(x, name)

  \S4method{$}{Octave}(x, name) <- value
}
\arguments{
  \item{x}{ object from which to extract element(s) or in
  which to replace element(s).  }

  \item{name}{ A literal character string or a \link{name}
  (possibly \link{backtick} quoted).  For extraction, this
  is normally (see under \sQuote{Environments}) partially
  matched to the \code{\link{names}} of the object.  }

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\description{
  \code{RcppOctave} provides a simple interface to Octave
  via the object \code{.O}, an instance of class
  \code{Octave}, that allows for direct access to Octave
  functions and variables using calls such as:
  \code{.O$svd(matrix(1:9,3))}.
}
\section{Methods}{
  \describe{

  \item{$}{\code{signature(x = "Octave")}: The method
  \code{$} provides a direct way of calling Octave
  functions or retrieving variables from Octave base
  context, via e.g. \code{.O$svd(x)} or \code{.O$a}. It is
  equivalent to \code{o_get(name)}. }

  \item{$<-}{\code{signature(x = "Octave")}: The method
  \code{$<-} allow to directly assign/set Octave variables
  via e.g. \code{.O$a <- 10}. }

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
\keyword{methods}

