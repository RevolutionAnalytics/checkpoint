#' Configure session as if it took place on snapshot date as far as packages are concerned.
#'
#' The aim of this function is configure a session as if it had occured right after the snapshot date. That way you can reproduce how a computation took place at a certain time. This is intended to support reproducibility, that is you only need to add a checkpoint call to your scripts or packages to make sure that subsequent updates to packages do not modify the results of running those scripts or using those packages.
#'
#' When you create a checkpoint, the following happens:
#'
#' As a consequence of running checkpoint, a specialized library will be set up that contains only packages as they were available on CRAN on the snapshot date; a session will only be able to install packages into or load them from said library. As an additional convenience, a heuristic is applied to find packages that are used in the project directory and install them in the snapshot specific library. Currently loaded packages are reloaded from the snapshot specific library.
#'
#'
#'
#' @param snapshotDate Date of snapshot to use in YYYY-MM-DD format,  e.g. "2014-06-20". .
#'
#' @param project A project  path. This is the path to the root of the project you want checkpointed. Defaults to current working directory per /code{/link{getwd}}.
#'
#'
#' @param verbose If TRUE, displays progress messages.
#'
#'
#' @return NULL. See the \code{details} section for side effects.
#'
#' @export
#'
#' @example /inst/examples/example_checkpoint.R
#'

checkpoint <- function(snapshotDate, project = getwd(), verbose=TRUE) {

  createFolders(snapshotDate)
  snapshoturl <- getSnapshotUrl(snapshotDate=snapshotDate)

  # set repos
  setMranMirror(snapshotUrl = snapshoturl)

  # Set lib path
  setLibPaths(snapshotDate)

  mssg(verbose, "Scanning for loaded pkgs")

  # Scan for packages used
  mssg(verbose, "Scanning for packages used in this project")
  packages.to.install = projectScanPackages(project)

  # download and install missing packages

  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this project ")
    utils::install.packages(pkgs = packages.to.install, verbose=FALSE, quiet=TRUE)
  } else {
    mssg(verbose, "No packages found to install")
  }

  # detach and reload checkpointed pkgs already loaded
  search.path = search()
  lapply(
    unlist(
      lapply(
        packages.to.install,
        grep,
        x = search.path)),
    function(x) {
      detach(x, unload = TRUE, force = TRUE)
      library(search.path[x], character.only = TRUE)})

  NULL}

setMranMirror <- function(snapshotDate, snapshotUrl = RRT:::getSnapShotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(snapshotDate, libPath=rrtPath(snapshotDate, "lib")){
  assign(".lib.loc", libPath, envir = environment(.libPaths))}

mranUrl <- function()"http://cran-snapshots.revolutionanalytics.com/"

getSnapshotUrl <- function(snapshotDate, url = mranUrl()){
  mran.root = url(url)
  on.exit(close(mran.root))
  tryCatch(
    suppressWarnings(readLines(mran.root)),
    error =
      function(e) {
        stop(sprintf("Unable to reach MRAN: %s", e$message))})
  snapshot.url = paste(gsub("/$", "", url), snapshotDate, sep = "/")
  con = url(snapshot.url)
  on.exit(close(con))
  tryCatch(
    suppressWarnings(readLines(con)),
    error =
      function(e) {
        stop("Unable to find snapshot on MRAN")})
  snapshot.url}


mssg <- function(x, ...) if(x) message(...)
