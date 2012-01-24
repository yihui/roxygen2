\name{rd_aspell_roclet}
\alias{rd_aspell_roclet}
\title{Roclet: Rd spell checker.}
\usage{
  rd_aspell_roclet()
}
\value{
  rd_aspell roclet
}
\description{
  The roclets executes \code{aspell} on all Rd files in the
  Rd directory, reports the number of possible
  misspellings, and writes the resulting data frame in the
  file \code{aspell.txt}.
}
\references{
  "Watch Your Spelling!" by Kurt Hornik and Duncan Murdoch,
  The R Journal Vol. 3/2, 2010-09-17.
}
\seealso{
  Other roclets: \code{\link{citation_roclet}},
  \code{\link{collate_roclet}}, \code{\link{demo_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}},
  \code{\link{version_roclet}}
}

