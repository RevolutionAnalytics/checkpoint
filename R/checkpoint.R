#' Set checkpoint date, downloads required packages from MRAN and sets libPath as well as CRAN mirror.
#'
#' The aim of the MRAN server combined with the RRT package is to serve as a "CRAN time machine".  Once a day, MRAN mirrors all of CRAN and saves a snapshot.  This allows you to install packages from a snapshot, and go back in time to this date, by installing packages as they were at that snapshot date.
#'
#' When you create a checkpoint, the following happens:
#'
#' \itemize{
#' \item{Create a snapshot folder to download packages. This library folder is at \code{~/.rrt}}
#' \item{Set options for your CRAN mirror to point to a MRAN snapshot, i.e. modify \code{options(repos)}}
#' \item{Scan your project folder for all packages used and install these using \code{\link[utils]{download.packages}}}
#' \item{Create a file \code{.Rprofile}} to initialise your library path to the snapshot library path
#' }
#'
#'
#'
#' @param snapshotDate (date) Required. Date of snapshot to use. E.g. "2014-06-20". If left blank, you
#' will be supplied with options.
#'
#' @param project A project  path. This is the path to the root of the project you want checkpointed. Defaults to current working directory per /code{/link{getwd}}.
#'
#' @param persistent If TRUE, adds library path to .Rprofile, else removes library path from .Rprofile
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
