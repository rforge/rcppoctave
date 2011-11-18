\docType{data}
\name{.O}
\alias{$}
\alias{$<-}
\alias{.DollarNames}
\alias{.DollarNames,Octave-method}
\alias{.O}
\alias{$<-,Octave-method}
\alias{$,Octave-method}
\title{Direct Interface to Octave}
\format{Formal class 'Octave' [package "test"] with 1 slots}
\description{
  \code{.O} in an instance of class \code{Octave} that
  allows for direct access to Octave functions and
  variables.

  The method \code{$} provides a direct way of calling
  Octave functions or retrieving variables from Octave base
  context, via e.g. \code{.O$svd(x)} or \code{.O$a}.

  The method \code{$<-} allow to directly assign/set Octave
  variables via e.g. \code{.O$a <- 10}.
}
\usage{
  .O
}
\section{Methods}{
\describe{
  \item{.DollarNames,Octave-method}{The method \code{$}
  provides a direct way of calling Octave functions or
  retrieving variables from Octave base context, via e.g.
  \code{.O$svd(x)} or \code{.O$a}. }

  \item{$<-,Octave-method}{The method \code{$<-} allow to
  directly assign/set Octave variables via e.g. \code{.O$a
  <- 10}. }
}
}
\examples{
.O$help()
.O$a <- 10
.O$a
}
\keyword{dataset}

