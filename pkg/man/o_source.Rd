\name{o_source}
\alias{o_source}
\title{Sourcing Octave/Matlab Files}
\usage{
  o_source(file)
}
\arguments{
  \item{file}{the path to the Octave/Matlab source file --
  typically with extension ".m".}
}
\value{
  None
}
\description{
  This function sources an Octave file within the current
  Octave session. The loaded functions are accessible by
  subsequent calls of \code{\link{.CallOctave}}.
}
\section{Octave Documentation for \emph{source}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(source,rd=TRUE)}

  \emph{[Generated from
  Octave-\Sexpr{RcppOctave::o_version()} on
  \Sexpr{Sys.time()}]}
}
\seealso{
  Other Octave_files: \code{\link{o_addpath}}
}

