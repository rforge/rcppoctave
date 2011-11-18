\name{packageEnv}
\alias{packageEnv}
\alias{packageName}
\alias{packagePath}
\title{Internal Utilities for Package Development}
\description{
  \code{packageEnv} returns the package's environment,
  which is its namespace in the case of an installed
  package, or its devtools environment.

  \code{packageName} returns the current package's name.

  \code{packagePath} returns the current package's root
  directory, which is its installation/loading directory in
  the case of an installed package, or its source directory
  served by devtools.
}
\usage{
  packageEnv()

  packageName()

  packagePath()
}
\value{
  an environment

  a character string

  a character string
}
\keyword{internal}

