#' Process a package with the Rd, namespace and collate roclets.
#'
#' This is the workhorse function that uses roclets, the built-in document
#' tranformation functions, to build all documentation for a package.  See
#' the documentation for the individual roclets, \code{\link{rd_roclet}},
#' \code{\link{namespace_roclet}} and \code{\link{collate_roclet}}, for
#' documentation on how to use each one.
#'
#' @param package.dir the package's top directory
#' @param roxygen.dir where to create roxygen output; defaults to
#'   \file{package.dir}.
#' @param copy.package copies the package over before adding/manipulating
#'    files.
#' @param overwrite overwrite target files?
#' @param unlink.target unlink target directory before processing files?
#' @param roclets character vector of roclet names to apply to package
#' @return \code{NULL}
#' @rdname roxygenize
#' @export
roxygenize <- function(package.dir,
                       roxygen.dir=package.dir,
                       copy.package=package.dir != roxygen.dir,
                       overwrite=TRUE,
                       unlink.target=FALSE,
                       roclets=c("collate", "namespace", "rd", "demo")) {

  skeleton <- c(roxygen.dir, file.path(roxygen.dir, c("man", "inst")))

  if (copy.package) {
    copy.dir(package.dir, roxygen.dir, unlink.target = unlink.target,
      overwrite = overwrite, verbose = FALSE)
  }

  for (dir in skeleton) {
    dir.create(dir, recursive=TRUE, showWarnings=FALSE)
  }

  roxygen.dir <- normalizePath(roxygen.dir)
  pkg <- package(roxygen.dir)

  roclets <- str_c(roclets, "_roclet", sep = "")
  for (roclet in roclets) {
    roc <- match.fun(roclet)(pkg)
    results <- roc_process(roc, roxygen.dir)
    roc_output(roc, results, roxygen.dir)
  }
}

#' @rdname roxygenize
#' @export
roxygenise <- roxygenize

# Recursively copy a directory thither; optionally unlinking
# the target first; optionally overwriting; optionally
# verbalizing.
#
# @note Not tested on non-linux platforms
copy.dir <- function(source,
                     target = source,
                     unlink.target=FALSE,
                     overwrite=FALSE,
                     verbose=FALSE) {
  if (unlink.target)
    unlink(target, recursive=TRUE)
  files <- list.files(source,
                      full.name=TRUE,
                      recursive=TRUE,
                      all.files=TRUE)
  for (source.file in files) {
    promoted.file <- sub('[^/\\]*(/|\\\\)', '', source.file)
    target.file <- file.path(target, promoted.file)
    target.dir <- dirname(target.file)
    ## Could create, instead, a list of unique directories in
    ## Theta(n).
    dir.create(target.dir, recursive=TRUE, showWarnings=FALSE)
    if (verbose)
      cat(sprintf('%s -> %s', source.file, target.file), '\n')
    file.copy(source.file, target.file, overwrite=overwrite)
  }
}
