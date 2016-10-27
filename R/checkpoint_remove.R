

#' List checkpoint archives on disk.
#' 
#' @inheritParams checkpoint
#' @export
#' @seealso checkpointRemove
#' @example inst/examples/example_remove.R
checkpointArchives <- function(checkpointLocation = "~/"){
  z <- list.files(path = paste0(normalizePath(checkpointLocation), ".checkpoint"), 
                  pattern = "\\d{4}-\\d{2}-\\d{2}", 
                  full.names = TRUE)
  normalizePath(z, winslash = "/")
}

#' Remove checkpoint archive from disk.
#' 
#' @inheritParams checkpoint
#' @export
#' @seealso checkpointArchives
#' @example inst/examples/example_remove.R
checkpointRemove <- function(snapshotDate, checkpointLocation = "~/"){
  z <- list.files(path = paste0(normalizePath(checkpointLocation), ".checkpoint"), 
                  pattern = paste0(snapshotDate, "$"), 
                  full.names = TRUE)
  to_delete <- normalizePath(z, winslash = "/")
  if(length(to_delete) == 0) {
    warning("archive not found")
    invisible(NULL)
  } else {
    res <- unlink(to_delete, recursive = TRUE)
    if(res == 0) message("successfully removed archive")
  }
}


