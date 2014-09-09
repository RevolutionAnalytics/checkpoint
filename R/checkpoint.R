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
#' @param repo A repository path. This is the path to the root of your RRT repository. Defaults to current working directory current working directory via \code{\link{getwd}}.
#' 
#' @param autosnap One of first, last or all. Determines how to resolve the snapshot id for a given date, if more than one snapshot exists on MRAN for that date.
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#'
#'
#' @return Files written to the user's machine, with informative messages on progress
#' 
#' @export
#' @family rrt
#' 
#' @example \inst\examples\example_checkpoint.R
#'

checkpoint <- function(snapshotdate=NULL, repo=getwd(), 
                       autosnap="last", verbose=TRUE) {
  
  createRepoFolders(repo)
  snapshoturl <- getSnapshotUrl(snapshotdate=snapshotdate, autosnap = autosnap)

  # set repos
  setMranMirror(snapshotUrl = snapshoturl)
  
  # Set lib path
  setLibPaths(repo)
  
  
  # Scan for packages used
  mssg(verbose, "Scanning for packages used in this repository")
  pkgsUsed <- repoScanPackages(repo)
  
  # Identify packages already installed
  
  # download and install missing packages
  
  if(length(pkgsUsed) > 0) {
    mssg(verbose, "Installing packages used in this repository")
    utils::install.packages(pkgs = pkgsUsed, repos=snapshoturl, verbose=FALSE, quiet=TRUE)
  } else {
    mssg(verbose, "No packages found to install")
  }
  
  # write / update manifest
  
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
setMranMirror <- function(snapshotdate, autosnap="last", 
                          snapshotUrl = getSnapShotUrl(snapshotdate, autosnap=autosnap)){
  options(repos = snapshotUrl)
}



#' Configure library path so that repo library is at top of search path.
#' 
#' @inheritParams checkpoint
#' @inheritParams repoInstalledPackages
setLibPaths <- function(repo, libPath=rrtPath(repo, "lib")){
  .libPaths(libPath)
}


#  ------------------------------------------------------------------------



mranUrl <- function()"http://cran-snapshots.revolutionanalytics.com/"


#' Retrieves snapshot id for a given snapshot date.
#' 
#' @inheritParams checkpoint
#' @param url MRAN snapshot URL
#' @param returnUrl If TRUE, returns URL with snapshotdate appended, otherwise returns snapshotdate as a string

#' @importFrom httr GET content
#' @importFrom XML xpathSApply htmlParse xmlValue
#' 
#' @export
#' 
#' @example \inst\examples\example_getSnapshotUrl.R
#' 
getSnapshotUrl <- function(snapshotdate, autosnap=c("first", "last", "all"), 
                           url = mranUrl(), returnUrl = TRUE){
  autosnap <- match.arg(autosnap)
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - Unable to download from MRAN", res$status_code))
  text <- content(res, as = "text")
  snaps <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  snaps <- gsub("/", "", snaps)
  if(!missing("snapshotdate") && !is.null(snapshotdate)) {
    snaps <- snaps[grep(snapshotdate, snaps)]
  }
  
  snapshotdates <- substr(snaps, 1, 10)
  res <- switch(autosnap, 
                first = tapply(snaps, snapshotdates, FUN=head, n=1),
                last  = tapply(snaps, snapshotdates, FUN=tail, n=1),
                all   = snaps
  )
  res <- unname(res)
  if(returnUrl) paste0(url, res) else res
}




#  ------------------------------------------------------------------------



#' Find out if a repository is an RRT repository.
#'
#' @inheritParams checkpoint
#' @export
#' @return A logical, TRUE or FALSE
#'
is_rrt <- function(repo) { file.exists(file.path(repo, "rrt")) }


#  ------------------------------------------------------------------------



#' Creates unique repo id from a digest of the file path.
#' 
#' @inheritParams checkpoint
#' 
#' @import digest
#' @keywords Internal
repoDigest <- function(repo){
  digest(normalizePath(repo, mustWork=FALSE))
}


mssg <- function(x, ...) if(x) message(...)
