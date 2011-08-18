#' @include parse-registry.R
NULL

register.preref.parsers(parse.value, 'demo')

#' Roclet: make demo/00index file.
#'
#' This roclet creates the demo/00index.
#'
#' Each demo file must specify one \code{@@demo} tag with a (multiline)
#' description of the demo.
#'
#' @family roclets
#' @return demo roclet
#' @examples
#' #' @demo Some examples which demonstrate the
#' #'   the demo roclet ...
#' NULL
#'
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
roc_process.demo <- function(roclet, partita, base_path, paths) {
  partita <- Filter(function(x) !is.null(x$demo), partita)

  index <- sapply(partita,
                  function(x) {
                    file <- x[[c("srcref", "filename")]]

                    c(name = sub("\\.[Rr]$", "", basename(file)),
                      description = gsub("\\n", " ", x$demo),
                      file = file)
                  })
  index <- t(index)

  if ( nrow(index) != length(paths) ) {
    no_demo <- setdiff(paths, index[, "file"])
    warning("Demo file(s) without @demo tag:\n",
            paste(no_demo, collapse = "\n"), call. = FALSE)

    # Set some generic description?
  }

  index
}

#' @S3method roc_output demo
roc_output.demo <- function(roclet, results, base_path) {
  INDEX <- file.path(base_path, "demo", "00index")

  if ( file.exists(INDEX) )
    unlink(INDEX)

  txt <- results[, c("name", "description"), drop = FALSE]
  txt <- apply(txt, 1, paste, collapse = "    ")

  writeLines(txt, INDEX)
}
