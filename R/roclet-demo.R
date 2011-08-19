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
demo_roclet <- function(package) {
  demo_files <- package$files("demo")
  demo_files <- grep(".[Rr]$", demo_files, value = TRUE)
  partita <- package$compute("demo_files", parse.files(demo_files))

  new_roclet(list(partita = partita, paths = demo_files), "demo")
}

#' @S3method roc_process demo
roc_process.demo <- function(roclet, base_path) {
  partita <- roclet$partita
  partita <- Filter(function(x) !is.null(x$demo), partita)

  paths <- roclet$paths

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
