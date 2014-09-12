#' Set MRAN checkpoint and create local project package library.
#'
#' This function decides what to do with your repository based on commands you give, and on skimming through your repo. You can run this function to start a new repository, without any work done yet, creating a new folder and RRT files, or you can initiate a RRT repository inside an existing project/folder you already have. If the latter, we don't alter your files at all, but simply write a few files needed for RRT to work properly.
#'
#' By detault initialization is done interactively, so that you can choose your settings or accept reasonable defaults.
#'
#' You can run this function to start a new RRT repo, and to refresh a repo with new work. This function downloads packages and installs them, as needed.
#'
#'
#' @param snapshotDate (date) Required. Date of snapshot to use. E.g. "2014-06-20". If left blank, you
#' will be supplied with options.
#'
#' @param repo A repository path. This is the path to the root of your RRT repository. Defaults to current working directory current working directory via /code{/link{getwd}}.
#'

#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#'
#'
#' @return Files written to the user's machine, with informative messages on progress
#'
#' @export
#' @family rrt
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
