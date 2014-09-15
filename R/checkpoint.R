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
#' @param repo A repository path. This is the path to the root of your RRT repository. Defaults to current working directory current working directory via /code{/link{getwd}}.
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

checkpoint <- function(snapshotDate=NULL, repo=getwd(), persistent = FALSE, verbose=TRUE) {

  createFolders(snapshotDate)
  snapshoturl <- getSnapshotUrl(snapshotDate=snapshotDate)

  # set repos
  setMranMirror(snapshotUrl = snapshoturl)

  # Set lib path
  setLibPaths(snapshotDate)

  mssg(verbose, "Scanning for loaded pkgs")

  # Scan for packages used
  mssg(verbose, "Scanning for packages used in this repository")
  packages.to.install = repoScanPackages(repo)

  # download and install missing packages

  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this repository")
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

  RProfilePath = file.path(repo, ".Rprofile")
  if(persistent) {
    # write .Rprofile in repo root folder
    ll = lapply(match.call()[-1], eval)
    checkpointLine =
      paste0(
        "RRT::",
        paste(capture.output(print(do.call(call, c(list("checkpoint"), ll)))), collapse = " "),
        " #RRT config")
    if(!file.exists(RProfilePath))
      file.create(RProfilePath)
    RProfile = readLines(RProfilePath)
    rrtLine = grep("#RRT config$", RProfile)
    if(length(rrtLine) == 0)
      RProfile = append(checkpointLine, RProfile)
    else
      RProfile[rrtLine] = checkpointLine
    writeLines(RProfile, RProfilePath)}
  else {
    if(file.exists(RProfilePath)) {
      RProfile = readLines(RProfilePath)
      rrtLine = grep("#RRT config$", RProfile)
      RProfile = RProfile[-rrtLine]
      writeLines(RProfile, RProfilePath)}}
  NULL}

setMranMirror <- function(snapshotDate, snapshotUrl = getSnapShotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(snapshotDate, libPath=rrtPath(snapshotDate, "lib")){
  assign(".lib.loc", libPath, envir = environment(.libPaths))}

mranUrl <- function()"http://cran-snapshots.revolutionanalytics.com/"

getSnapshotUrl <- function(snapshotDate, url = mranUrl()){
  url = paste(gsub("/$", "", url), snapshotDate, sep = "/")
  con = url(url)
  readLines(con)
  url}

mssg <- function(x, ...) if(x) message(...)
