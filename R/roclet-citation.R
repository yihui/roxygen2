#' @include parse-registry.R
#' @import stringr
#' @import utils
NULL



#' Roclet: make inst/CITATION file.
#'
#' This roclet creates the inst/CITATION file based on a
#' inst/CITATION.bib file.
#'
#' @family roclets
#' @return citation roclet
#' @export
citation_roclet <- function() {
  new_roclet(list(), "citation")
}



#' @S3method roc_process citation
roc_process.citation <- function(roclet, partita, base_path) {
  as_text <- function(cit) {
    txt_version <- format(cit)

    cit <- unclass(cit)[[1]]
    author <- cit$author
    header <- cit$citentryheader
    footer <- cit$citentryfooter
    cit$author <- cit$citentryheader <- cit$citentryfooter <- NULL

    txt_author <- sprintf("as.person(%s)", dQuote(as.character(author)))
    txt_author <- paste(txt_author, collapse = ", ")
    txt_author <- sprintf("author = personList(%s)", txt_author)

    txt <- paste(names(cit), "=", dQuote(cit))
    txt <- append(txt, txt_author)
    txt <- append(txt, sprintf("textVersion = %s", sQuote(txt_version)))
    txt <- append(txt, sprintf("entry = %s", dQuote(attr(cit, "bibtype"))))

    if ( !is.null(header) )
      txt <- append(txt, sprintf("header = %s", dQuote(header)))

    if ( !is.null(footer) )
      txt <- append(txt, sprintf("footer = %s", dQuote(footer)))

    txt
  }

  lapply(partita, as_text)
}



#' @S3method roc_output citation
roc_output.citation <- function(roclet, results, base_path) {
  CITATION <- file.path(base_path, "inst", "CITATION")

  cat("Creating", CITATION, "\n")

  if ( file.exists(CITATION) )
    unlink(CITATION)

  txt <- lapply(results, paste, collapse = ", \n")
  txt <- lapply(txt, function(x) sprintf("citEntry(\n%s\n)", x))
  txt <- paste(txt, collapse= "\n\n")

  writeLines(txt, CITATION)
}



roc_setup.citation <- function(package.dir, roxygen.dir) {
  stopifnot(require("bibtex"))
  bib <- file.path(roxygen.dir, "inst", "CITATION.bib")
  read.bib(bib)
}
