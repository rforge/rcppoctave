\name{packageEnv}
\alias{getLoadingNamespace}
\alias{packageEnv}
\alias{packageName}
\alias{packagePath}
\title{Internal Utilities for Package Development}
\usage{
  packageEnv()

  getLoadingNamespace(env = FALSE, info = FALSE)

  packageName()

  packagePath()
}
\arguments{
  \item{env}{logical that indicates that the namespace's
  environment (i.e. the namespace itself) should be
  returned.}

  \item{info}{logical that indicate that the complete
  information list should be returned}
}
\value{
  an environment

  the name of the loading namespace if \code{env} and
  \code{info} are \code{FALSE}, an environment if
  \code{env=TRUE}, a list with elements \code{pkgname} and
  \code{libname} if \code{info=TRUE}.

  a character string

  a character string
}
\description{
  \code{packageEnv} returns the package's environment,
  which is its namespace in the case of an installed
  package, or its devtools environment.

  \code{getLoadingNamespace} returns information about the
  loading namespace. It is a wrapper to
  \code{\link{loadingNamespaceInfo}}, that does not throw
  an error.

  \code{packageName} returns the current package's name.

  \code{packagePath} returns the current package's root
  directory, which is its installation/loading directory in
  the case of an installed package, or its source directory
  served by devtools.
}
\keyword{internal}

