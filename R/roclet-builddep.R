#' @include parse-registry.R
#' @import stringr
NULL

#' Roclet: make Build-Depends field in DESCRIPTION.
#'
#' Tries to detect packages which are used during the
#' package development using file and session info.
#'
#' @family roclets
#' @return builddep roclet
#' @export
builddep_roclet <- function(package) {
  files <- package$files(flat = FALSE)
  session <- sessionInfo()

  new_roclet(list(files = files, session = session), "builddep")
}

#' @S3method roc_process builddep
roc_process.builddep <- function(roclet, base_path) {
  pkgs <- c("roxygen2",
            ## Generalize to something like
            ## ls(pattern = "_detector") or similar?
            unittest_detector(roclet$files, roclet$session),
            devtools_detector(roclet$files, roclet$session))
  sapply(pkgs, package_info)
}

#' @S3method roc_output builddep
roc_output.builddep <- function(roclet, results, base_path) {
  DESCRIPTION <- file.path(base_path, "DESCRIPTION")
  old <- read.description(DESCRIPTION)
  new <- old
  new[["Build-Depends"]] <- str_c(results, collapse = ", ")
  write.description(new, DESCRIPTION)
}

unittest_detector <- function(files, session) {
  pkgs <- c("RUnit", "testthat")
  test_files <- unlist(files[["tests"]])

  if ( is.null(test_files) )
    return(character())

  check <- sapply(test_files, file_grepl, pkgs)
  check <- apply(check, 1, any)

  pkgs[check]
}

devtools_detector <- function(files, session) {
  pkgs <- c("devtools")

  check <- pkgs %in% names(session$otherPkgs)

  pkgs[check]
}

file_grepl <- function(file, patterns) {
  raw <- readLines(file)
  patterns %in% str_match(raw, paste(patterns, collapse = "|"))
}

package_info <- function(name) {
  sprintf("%s (= %s)", name, packageDescription(name)$Version)
}
