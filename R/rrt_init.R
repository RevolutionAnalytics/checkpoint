#' Initiate a RRT repository. 
#' 
#' This function initiates a repository.
#'
#' @import digest
#' @export
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param verbose
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo. (Default)
#' 
#' @return Files written to the user system, with informative messages on progress
#' @examples \dontrun{
#' rrt_init(repo="~/newrepo")
#' }

rrt_init <- function(repo, verbose=TRUE, rprofile=NULL)
{
  # create r
  repoid <- digest(repo)
  
  # Write to .Renviron (or .Rprofile?) the path to the list of RRT repositories
  ###
  
  # create repo
  if(is.null(repo)) repo <- getwd()
  if(!file.exists(repo)){ # only create if file doesn't exist already
    mssg(verbose, sprintf("Creating repository %s", repo))
    dir.create(repo)
  }
  
  # check for rrt directory in the repo, and create if doesn't exist already
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)
  if(!all(grepl("rrt", present))){
    mssg(verbose, sprintf("Creating rrt directory %s", lib))
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Look for packages in the project
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE)
  
  # install packages in a private location for this project
#   installPkgs()
  
  # Write to internal manifest file
  mssg(verbose, "Writing repository info file")
  writeManifest(repo, lib, pkgs)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    libpaths <- sprintf('.libPaths("%s")', lib)
    startupmssg <- sprintf("cat('Starting repo from RRT repository: %s')", repoid)
    cat(c(libpaths, startupmssg), file=rprofilepath, sep="\n")
  } else {
    NULL # fixme: add ability to write options to the rprofile file
  }
  
  message("RRT initialization completed.")
}

#' Function to install pkgs
#' 
#' @import miniCRAN
#' @export
#' @param x
#' @param lib
#' @param recursive
#' @examples \dontrun{
#' installPkgs()
#' }
installPkgs <- function(x, lib, recursive=FALSE){
  # FIXME, needs some fixes on miniCRAN to install source if binaries not avail.
  makeRepo(pkgs = x, path = lib, download = TRUE)
}

#' Function to get system requireqments, if any, from installed packages
#' 
#' @export
#' @param x (character) Name of package. One to many in a vector or list
#' @keywords internal
#' @examples
#' getsysreq('RCurl')
#' getsysreq(c('RCurl','doMC','ggplot2','XML','rgdal'))

getsysreq <- function(x)
{
  tmp <- 
    lapply(x, function(y){
      res <- packageDescription(y, encoding = NA)
      if(is(res, "packageDescription")) res$SystemRequirements
    })
  names(tmp) <- x
  tmp
}

#' Function to write manifest file
#' 
#' @export
#' @keywords internal
#' @return Writes a RRT manifest file to disc
writeManifest <- function(repository, librar, packs, repoid){
  infofile <- file.path(repository, "rrt", "rrt_manifest.txt")
  installedwith <- "InstalledWith: RRT"
  installedfrom <- "InstalledFrom: source"
  rrtver <- sprintf("RRT_version: %s", packageVersion("RRT"))
  rver <- sprintf("R_version: %s", paste0(as.character(R.version[c('major','minor')]), collapse="."))
  pkgsloc <- sprintf("PkgsInstalledAt: %s", librar)
  sysreq <- sprintf("SystemRequirements: %s", rtt_compact(getsysreq(packs)))
  pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ","))
  repositoryid <- sprintf("RepoID: %s", repoid)
  info <- c(installedwith, installedfrom, rrtver, rver, path.expand(pkgsloc), repositoryid, pkgs_deps)
  cat(info, file = infofile, sep = "\n")
}