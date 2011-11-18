\name{o_who}
\alias{o_who}
\title{Listing Octave Variables}
\description{
  Lists currently defined variables in Octave global
  context.
}
\usage{
  o_who(..., options)
}
\arguments{
  \item{...}{filtering patterns. Only names matching any of
  the patterns are returned.}

  \item{options}{options passed to Octave function
  \code{who}. See section \emph{Octave Documentation}.}
}
\value{
  None
}
\section{Octave Documentation for \emph{who}}{
  \Sexpr[results=rd,stage=render]{RcppOctave::o_help(who,rd=TRUE)}
}
\examples{
\dontshow{ o_clear() }
o_who()
l <- as.list(setNames(1:10, letters[1:10]))
o_assign(l)
o_who()
\dontshow{  stopifnot( identical(o_who(), names(l)) ) }

prefnames <- paste('pref', letters[1:10], sep='')
o_assign( setNames(l, prefnames) )
o_who()
o_who('pref*')
\dontshow{  stopifnot( identical(o_who('pref*'), prefnames) ) }
}

