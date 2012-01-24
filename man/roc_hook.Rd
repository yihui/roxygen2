\name{roc_hook}
\alias{roc_hook}
\title{Roclet hook specification.}
\usage{
  roc_hook(name, point, setup, roclets)
}
\arguments{
  \item{name}{A descriptive hook name}

  \item{point}{An entry point for the hook, i.e., the file
  path which must be available in the package directory to
  execute the hook (e.g., the R directory, the DESCRIPTION
  file, a specific Rd file)}

  \item{setup}{A setup function which preprocesses the
  files needed for the roclets of the hook}

  \item{roclets}{The roclets executed in this hook}
}
\description{
  Roclet hook specification.
}
\examples{
## The following hook is called "base" and executes the
  ## setup function "roc_setup.rd" and then its roclets "collate",
  ## "namespace", and "rd" if there is the specified entry point,
  ## the directory "R", available:
  roc_hook(name = "base",
           point = "R",
           setup = "roc_setup.rd",
           roclets = c("collate", "namespace", "rd"))
}

