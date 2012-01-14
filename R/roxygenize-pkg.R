#' @include roxygenize.R
{}



#' @param name A descriptive hook name
#' @param point An entry point for the hook, i.e., the file path which
#'   must be available in the package directory to execute the hook
#'   (e.g., the R directory, the DESCRIPTION file, a specific Rd file)
#' @param setup A setup function which preprocesses the files needed
#'   for the roclets of the hook
#' @param roclets The roclets executed in this hook
roc_hook <- function(name, point, setup, roclets) {
  list(name = name, point = point, setup = setup, roclets = roclets)
}



default_hooks <- function() {
  list(roc_hook("base", "R", "parse_r_files", c("collate", "namespace", "rd")))

  #list(roc_hook("base", "R", "R", c("collate", "namespace", "rd")),
  #     roc_hook("demoindex", "demo", "demo", c("demo")),
  #     roc_hook("citationfile", "inst/references.bib", "references", c("citation")))
}



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
    cat(sprintf("Applying %s hook ... ", sQuote(hook$name)))

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
    parsed <- match.fun(setup)(package.dir, roxygen.dir)

    for (roclet in roclets) {
      roc <- match.fun(roclet)()
      results <- roc_process(roc, parsed, roxygen.dir)
      roc_output(roc, results, roxygen.dir)
    }
  }
}


