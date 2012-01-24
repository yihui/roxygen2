#' @include roxygenize.R
{}



#' Roclet hook specification.
#'
#' @param name A descriptive hook name
#' @param point An entry point for the hook, i.e., the file path which
#'   must be available in the package directory to execute the hook
#'   (e.g., the R directory, the DESCRIPTION file, a specific Rd file)
#' @param setup A setup function which preprocesses the files needed
#'   for the roclets of the hook
#' @param roclets The roclets executed in this hook
#'
#' @examples
#'   ## The following hook is called "base" and executes the
#'   ## setup function "roc_setup.rd" and then its roclets "collate",
#'   ## "namespace", and "rd" if there is the specified entry point,
#'   ## the directory "R", available:
#'   roc_hook(name = "base",
#'            point = "R",
#'            setup = "roc_setup.rd",
#'            roclets = c("collate", "namespace", "rd"))
#'
#' @export
roc_hook <- function(name, point, setup, roclets) {
  list(name = name, point = point, setup = setup, roclets = roclets)
}



#' Default roclet hooks.
#' @export
default_hooks <- function() {
  list(roc_hook("base", "R", "roc_setup.rd", c("collate", "namespace", "rd")),
       roc_hook("demo", "demo", "roc_setup.demo", "demo"),
       roc_hook("citation", "inst/CITATION.bib", "roc_setup.citation", "citation"),
       roc_hook("version", "NEWS", "roc_setup.version", "version"),
       roc_hook("aspell", "man", "roc_setup.rd_aspell", "rd_aspell"))
}



#' Process a package using roclet hooks.
#'
#' Hooks allow the specification of entry points, i.e., file paths in
#' a package structure, and consequently the execution of the hook if the
#' entry point is available. See \code{\link{roc_hook}} for the specification
#' of own hooks; and \code{\link{default_hooks}} for the default hooks.
#'
#' @param package.dir the package's top directory
#' @param roxygen.dir where to create roxygen output; defaults to
#'   \file{package.dir}.
#' @param copy.package copies the package over before adding/manipulating
#'    files.
#' @param overwrite overwrite target files?
#' @param unlink.target unlink target directory before processing files?
#' @param hooks a list of hooks; see \code{\link{roc_hook}}
#'
#' @export
roxygenize_pkg <- function(package.dir,
                           roxygen.dir = package.dir,
                           copy.package = package.dir != roxygen.dir,
                           overwrite = TRUE,
                           unlink.target = FALSE,
                           hooks = default_hooks()) {

  skeleton <- c(roxygen.dir, file.path(roxygen.dir, c("man", "inst")))

  if (copy.package) {
    copy.dir(package.dir, roxygen.dir, unlink.target = unlink.target,
      overwrite = overwrite, verbose = FALSE)
  }

  for (dir in skeleton) {
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
  }

  roxygen.dir <- normalizePath(roxygen.dir)

  for ( hook in hooks ) {
    cat(sprintf("* Applying %s hook ... ", sQuote(hook$name)))

    ## Refresh package content to allow roclets on
    ## output created by previous roclets:
    content <- dir(file.path(roxygen.dir), recursive = TRUE,
                   include.dirs = TRUE)

    if ( all(hook$point != content) ) {
      cat("no\n")
      next
    }

    cat("yes\n")

    roclets <- str_c(hook$roclets, "_roclet", sep = "")
    parsed <- match.fun(hook$setup)(package.dir, roxygen.dir)

    for (roclet in roclets) {
      roc <- match.fun(roclet)()
      results <- roc_process(roc, parsed, roxygen.dir)
      roc_output(roc, results, roxygen.dir)
    }
  }
}


