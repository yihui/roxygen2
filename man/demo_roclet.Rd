\name{demo_roclet}
\alias{demo_roclet}
\title{Roclet: make demo/00index file.}
\usage{
  demo_roclet()
}
\value{
  demo roclet
}
\description{
  This roclet creates the demo/00index file.
}
\details{
  Each demo file must specify one \code{@demo} tag with a
  (multiline) description of the demo. The demo file name
  and this description is then used to create the entry in
  the 00index file.

  The roclet supports a \code{@details} tag as well, but it
  is currently ignored.
}
\examples{
roclet <- demo_roclet()
\dontrun{
  roc_proc(roclet, dir('demo'))
  roc_out(roclet, dir('demo'), "demo")
}
}
\seealso{
  Other roclets: \code{\link{collate_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}}
}

