#' Set MRAN checkpoint and create local project package library.
#'
#' This function decides what to do with your repository based on commands you give, and on skimming through your repo. You can run this function to start a new repository, without any work done yet, creating a new folder and RRT files, or you can initiate a RRT repository inside an existing project/folder you already have. If the latter, we don't alter your files at all, but simply write a few files needed for RRT to work properly.
#'
#' By detault initialization is done interactively, so that you can choose your settings or accept reasonable defaults.
#'
#' You can run this function to start a new RRT repo, and to refresh a repo with new work. This function downloads packages and installs them, as needed.
#'
#'
#' @param snapshotdate (date) Required. Date of snapshot to use. E.g. "2014-06-20". If left blank, you
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

checkpoint <- function(snapshotdate=NULL, repo=getwd(), verbose=TRUE) {

  createRepoFolders(repo)
  snapshoturl <- getSnapshotUrl(snapshotdate=snapshotdate)

  # set repos
  setMranMirror(snapshotUrl = snapshoturl)

  # Set lib path
  setLibPaths(repo)

  mssg(verbose, "Scanning for loaded pkgs")

  untouchables = c("RRT", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")
  packages.to.install =
    setdiff(
      do.call(rbind, strsplit(grep("^package:", search(), value = TRUE), ":"))[,2],
      untouchables)

  # Scan for packages used
  mssg(verbose, "Scanning for packages used in this repository")
  packages.to.install = c(repoScanPackages(repo), packages.to.install)

  # Identify packages already installed

  # download and install missing packages

  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this repository or currently loaded")
    utils::install.packages(pkgs = packages.to.install, verbose=FALSE, quiet=TRUE)
  } else {
    mssg(verbose, "No packages found to install")
  }

lapply(
  packages.to.install,
  function(x) {
    detach(paste0("package:", x), unload = TRUE, force = TRUE)
    library(x, character.only = TRUE)})

# write .Rprofile in repo root folder


}



#  ------------------------------------------------------------------------

#' Return matrix of DESCRIPTION information of all packages installed in repo.
#'
#' @inheritParams checkpoint
#'
#' @param libPath (character) Location of package library in repo
#'
#' @export
repoInstalledPackages <- function(repo, libPath = rrtPath(repo, "lib")){
  rbind.matrix <- function (..., nms) {
    dfs <- list(...)[[1]]
    if(missing(nms)) nms <- unique(names(unlist(dfs)))
    dfs[] <- lapply(dfs, function(x){
      missing <- setdiff(nms, names(x))
      x[missing] <- NA
      x
    })
    out <- do.call(rbind, lapply(dfs, `[`, nms))
    colnames(out) <- nms
    out
  }

  desc <- list.files(libPath, pattern = "DESCRIPTION", full.names = TRUE, recursive = TRUE)
  nms <- c("Package", "Version", "License", "SystemRequirements", "Title", "Date", "Date/Publication")
  if(length(desc) > 0){
    dcf <- lapply(desc, read.dcf, all=TRUE)
    rbind.matrix(dcf, nms)
  } else {
    NULL
  }
}


#  ------------------------------------------------------------------------



#' Sets CRAN mirror to MRAN snapshot.
#'
#' @inheritParams checkpoint
#' @inheritParams getSnapshotUrl
#'
#' @param snapshotUrl URL for CRAN snapshot mirror
#'
#' @export
setMranMirror <- function(snapshotdate, snapshotUrl = getSnapShotUrl(snapshotdate)){
  options(repos = snapshotUrl)
}


setLibPaths <- function(repo, libPath=rrtPath(repo, "lib")){
  assign(".lib.loc", libPath, envir = environment(.libPaths))}


#  ------------------------------------------------------------------------


#' @importFrom httr GET build_url parse_url

mranUrl <- function()"http://cran-snapshots.revolutionanalytics.com/"


getSnapshotUrl <- function(snapshotdate, url = mranUrl()){
  url = parse_url(url)
  url$path = gsub("^/", "", gsub("/+", "/", paste(url$path, snapshotdate, sep = "/")))
  url = build_url(url)
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - Unable to download from MRAN", res$status_code))
  url}


#  ------------------------------------------------------------------------




#' Creates unique repo id from a digest of the file path.
#'
#' @inheritParams checkpoint
#'
#' @importFrom digest digest
#' @keywords Internal
repoDigest <- function(repo){
  digest(normalizePath(repo, mustWork=FALSE))
}


mssg <- function(x, ...) if(x) message(...)
