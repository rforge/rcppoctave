\name{o_source}
\alias{o_source}
\title{Sourcing Octave/Matlab Files}
\description{
  This function sources an Octave file within the current
  Octave session. The loaded functions are accessible by
  subsequent calls of \code{\link{.CallOctave}}.
}
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
\section{Octave Documentation for \emph{source}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(source,rd=TRUE)}
}
\seealso{
  Other Octave_files: \code{\link{o_addpath}}
}

