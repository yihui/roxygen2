\name{roxygenize_pkg}
\alias{roxygenize_pkg}
\title{Process a package using roclet hooks.}
\usage{
  roxygenize_pkg(package.dir, roxygen.dir = package.dir,
    copy.package = package.dir != roxygen.dir,
    overwrite = TRUE, unlink.target = FALSE,
    hooks = default_hooks())
}
\arguments{
  \item{package.dir}{the package's top directory}

  \item{roxygen.dir}{where to create roxygen output;
  defaults to \file{package.dir}.}

  \item{copy.package}{copies the package over before
  adding/manipulating files.}

  \item{overwrite}{overwrite target files?}

  \item{unlink.target}{unlink target directory before
  processing files?}

  \item{hooks}{a list of hooks; see \code{\link{roc_hook}}}
}
\description{
  Hooks allow the specification of entry points, i.e., file
  paths in a package structure, and consequently the
  execution of the hook if the entry point is available.
  See \code{\link{roc_hook}} for the specification of own
  hooks; and \code{\link{default_hooks}} for the default
  hooks.
}

