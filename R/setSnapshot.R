#' Set default CRAN repository to MRAN snapshot date.
#'
#' @inheritParams checkpoint
#'
#' @export
#' @example /inst/examples/example_setSnapshot.R
#'
setSnapshot <- function(snapshotDate){
  if (missing(snapshotDate) || is.null(snapshotDate)) return(getOption("repos"))
  repoDate <- paste0(mranUrl(), snapshotDate)
  setDownloadOption()
  response <- tryCatch(
    suppressWarnings(readLines(repoDate)),
    error = function(e)e
  )
  if(inherits(response, "error")) stop(paste0("Invalid snapshot date."))
  options(repos = c(CRAN = repoDate))
  message(paste("Using CRAN mirror at", repoDate))
}
