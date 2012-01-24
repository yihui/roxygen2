#' @include parse-registry.R
#' @import stringr
#' @import tools
NULL



#' Roclet: Rd spell checker.
#'
#' The roclets executes \code{aspell} on all Rd files in the
#' Rd directory, reports the number of possible misspellings,
#' and writes the resulting data frame in the file
#' \code{aspell.txt}.
#'
#' @references
#'   "Watch Your Spelling!" by Kurt Hornik and Duncan Murdoch,
#'   The R Journal Vol. 3/2, 2010-09-17.
#'
#' @family roclets
#' @return rd_aspell roclet
#' @export
rd_aspell_roclet <- function() {
  new_roclet(list(), "rd_aspell")
}



#' @S3method roc_process rd_aspell
roc_process.rd_aspell <- function(roclet, partita, base_path) {
  aspell(partita, "Rd")
}



#' @S3method roc_output rd_aspell
roc_output.rd_aspell <- function(roclet, results, base_path) {
  results$Suggestions <- NULL

  if ( nrow(results) == 0 )
    return(NULL)

  if ( file.exists("aspell.txt") )
    unlink("aspell.txt")

  cat("Detecting", nrow(results), "possible misspellings; see",
      sQuote("aspell.txt"), "\n")

  write.table(results, file = "aspell.txt")
}



roc_setup.rd_aspell <- function(package.dir, roxygen.dir) {
  dir(file.path(roxygen.dir, "man"), ".Rd$", full.names = TRUE)
}
