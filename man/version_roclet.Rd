\name{version_roclet}
\alias{version_roclet}
\title{Roclet: make Version and Date fields in DESCRIPTION.}
\usage{
  version_roclet()
}
\value{
  version roclet
}
\description{
  This roclet reades the version and the date from the
  latest NEWS file and writes the corresponding fields in
  the DESCRIPTION file. If the date is empty, the the
  current date is set.
}
\seealso{
  Other roclets: \code{\link{citation_roclet}},
  \code{\link{collate_roclet}}, \code{\link{demo_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}}
}

