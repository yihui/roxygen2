# Set up package abstraction for roclets.
#
# There is duplicated code from \code{cache.R}.
#
# @param roxygen.dir the package's top directory
# @return list with available accessor functions
#   for package information
package <- function(roxygen.dir) {
  cache <- new.env(parent = emptyenv())

  compute <- function(keys, code) {
    hash <- suppressWarnings(digest(keys))
    if (exists(hash, cache, inherits = FALSE)) {
      return(cache[[hash]])
    }

    (cache[[hash]] <- force(code))
  }

  reset <- function() {
    cache <<- new.env(parent = emptyenv())
  }

  close <- function() {
    cache <<- NULL
  }


  ## Precomputed package information:
  cache$.files <- dir_list(roxygen.dir)
  cache$.files[["R"]] <- r_files_ordered(cache$.files, roxygen.dir)

  files <- function(what = NULL) {
    if ( is.null(what) )
      files <- cache$.files
    else
      files <- cache$.files[[what]]

    unname(unlist(files))
  }

  list(files = files, compute = compute, reset = reset, close = close)
}


dir_list <- function(dir) {
  files_list <- list()

  files <- dir(file.path(dir), full.names = TRUE,
               recursive = TRUE, include.dirs = TRUE)
  isdir <- file.info(files)$isdir

  ## Directory hierarchy:
  dirs <- files[isdir]
  dirs <- strsplit(dirs, "/")

  max_depth <- max(sapply(dirs, length))

  for ( i in seq(length = max_depth) ) {
    i_dirs <- unique(lapply(dirs, "[", 1:i))
    i_dirs <- Filter(function(x) !any(is.na(x)), i_dirs)

    for ( d in i_dirs )
      files_list[[d]] <- list()
  }

  ## Files:
  files <- files[!isdir]

  filenames <- basename(files)
  paths <- dirname(files)
  paths <- strsplit(paths, "/")

  for ( i in seq(along = paths) )
    files_list[[paths[[i]]]][[filenames[i]]] <- files[i]

  files_list[[1]]
}


r_files_ordered <- function(files_list, roxygen.dir) {
  r_files <- files_list[["R"]]

  if (!is.null(files_list[["DESCRIPTION"]])) {
    DESCRIPTION <- files_list[["DESCRIPTION"]]

    desc <- read.description(DESCRIPTION)
    raw_collate <- desc$Collate %||% ""
    con <- textConnection(raw_collate)
    on.exit(close(con))
    collate <- scan(con, "character", sep = " ", quiet = TRUE)

    collate_path <- file.path(roxygen.dir, "R", collate)
    collate_exists <- Filter(file.exists, collate_path)
    r_files <- c(collate_exists, setdiff(r_files, collate_exists))
    names(r_files) <- basename(unlist(r_files))
    # load the dependencies
    pkgs <- paste(c(desc$Depends, desc$Imports), collapse = ", ")
    if (pkgs != "") {
      pkgs <- gsub("\\s*\\(.*?\\)", "", pkgs)
      pkgs <- strsplit(pkgs, ",")[[1]]
      pkgs <- gsub("^\\s+|\\s+$", "", pkgs)
      lapply(pkgs[pkgs != "R"], require, character.only = TRUE)
    }
  }

  r_files
}
