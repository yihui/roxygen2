#' @include parse-registry.R
#' @import stringr
NULL



register.preref.parsers(parse.value, "demo")
register.preref.parsers(parse.value, "details")



#' Roclet: make demo/00index file.
#'
#' This roclet creates the demo/00index file.
#'
#' @details
#' Each demo file must specify one \code{@@demo} tag with a (multiline)
#' description of the demo. The demo file name and this description is
#' then used to create the entry in the 00index file.
#'
#' The roclet supports a \code{@@details} tag as well, but it is
#' currently ignored.
#'
#' @family roclets
#' @return demo roclet
#'
#' @examples
#' roclet <- demo_roclet()
#' \dontrun{
#'   roc_proc(roclet, dir('demo'))
#'   roc_out(roclet, dir('demo'), "demo")
#' }
#' @export
demo_roclet <- function() {
  new_roclet(list(), "demo")
}



#' @S3method roc_process demo
roc_process.demo <- function(roclet, partita, base_path) {
  files <- unique(sapply(partita, function(x) x$srcref$filename))
  partita <- Filter(function(x) !is.null(x$demo), partita)

  value <- function(x) {
    y <- ""
    if ( !is.null(x) ) {
      y <- str_replace_all(x, "\\n", " ")
      y <- str_replace_all(y, "\\s+", " ")
    }
    y
  }

  index <- lapply(partita,
                  function(x) {
                    file <- x[[c("srcref", "filename")]]

                    c(name = sub("\\.[Rr]$", "", basename(file)),
                      demo = value(x$demo),
                      details = value(x$details),
                      file = file)
                  })

  if ( length(index) != length(files) ) {
    no_demo <- setdiff(files, sapply(index, "[[", "file"))
    warning("Demo file(s) without @demo tag: ",
            paste(basename(no_demo), collapse = ", "), call. = FALSE)
    ## TODO: Set some generic description?
  }

  index
}



#' @S3method roc_output demo
roc_output.demo <- function(roclet, results, base_path) {
  if ( length(results) == 0 )
    return(NULL)

  INDEX <- file.path(base_path, "demo", "00index")

  cat("Creating ", INDEX, file, "\n")

  if ( file.exists(INDEX) )
    unlink(INDEX)

  txt <- do.call(rbind, results)
  txt <- txt[, c("name", "demo"), drop = FALSE]
  txt <- apply(txt, 1, paste, collapse = "    ")

  writeLines(txt, INDEX)
}



roc_setup.demo <- function(package.dir, roxygen.dir) {
  demo_files <- dir(file.path(roxygen.dir, "demo"), "[.Rr]$", full.names = TRUE)
  parse.files(demo_files)
}
