#' @include parse-registry.R
#' @import stringr
NULL



#' Roclet: make Version and Date fields in DESCRIPTION.
#'
#' This roclet reades the version and the date from the
#' latest NEWS file and writes the corresponding fields
#' in the DESCRIPTION file. If the date is empty, the
#' the current date is set.
#'
#' @family roclets
#' @return version roclet
#' @export
version_roclet <- function() {
  new_roclet(list(), "version")
}



#' @S3method roc_process version
roc_process.version <- function(roclet, partita, base_path) {
  ## We assume that the first entry is the latest entry.

  version <- partita[1, "Version"]
  date <- partita[1, "Date"]
  date <- ifelse(is.na(date), as.character(Sys.Date()))

  list(version = version, date = date)
}



#' @S3method roc_output version
roc_output.version <- function(roclet, results, base_path) {
  DESCRIPTION <- file.path(base_path, "DESCRIPTION")
  old <- read.description(DESCRIPTION)
  new <- old
  new$Version <- results$version
  new$Date <- results$date
  write.description(new, DESCRIPTION)

  if (!identical(old, read.description(DESCRIPTION))) {
    cat('Updating version and date directive in ', DESCRIPTION, "\n")
  }
}



roc_setup.version <- function(package.dir, roxygen.dir) {
  NEWS <- file.path(roxygen.dir, "NEWS")
  reader <- tools:::.news_reader_default
  reader(NEWS)
}
