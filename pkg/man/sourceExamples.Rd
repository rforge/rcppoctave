\name{sourceExamples}
\alias{sourceExamples}
\title{Loading Example M-files}
\usage{
  sourceExamples(file = "example.m")
}
\arguments{
  \item{file}{filename of the example script to source}
}
\description{
  Source an example M-file in the sub-directory
  \dQuote{scripts/} of RcppOctave installation.
}
\examples{
sourceExamples()
sourceExamples('ex_source.m')
}

