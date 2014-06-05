#' Initiate a RRT repository. 
#' 
#' This function initiates a repository.
#'
#' @import digest miniCRAN
#' @export
#' @template rrt
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo.
#' 
#' @return Files written to the user system, with informative messages on progress
#' @examples \dontrun{
#' rrt_init(repo="~/testrepo")
#' rrt_refresh(repo="~/testrepo")
#' }

rrt_init <- function(repo, verbose=TRUE, rprofile=NULL)
{
  # create repo id using digest
  repoid <- digest(repo)
  if(is.null(repo)) repo <- getwd()
  
  # Write to .Renviron (or .Rprofile?) the path to the list of RRT repositories
  ###
  
  # create repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    mssg(verbose, sprintf("Creating repository %s", repo))
    dir.create(repo)
  }
  
  # check for rrt directory in the repo, and create if doesn't exist already
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)
  if(!all(grepl("rrt", present))){
    mssg(verbose, sprintf("Creating rrt directory %s", lib))
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  }
  
  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE)
  
  # install packages in a private location for this project
  installPkgs(pkgs, lib, verbose)
  
  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repo, lib, pkgs, repoid)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    libpaths <- sprintf('.libPaths("%s")', lib)
    startupmssg <- sprintf("cat('Starting repo from RRT repository: %s')", repoid)
    cat(c(libpaths, startupmssg), file=rprofilepath, sep="\n")
  } else {
    NULL # fixme: add ability to write options to the rprofile file
  }
  
  message("\n>>> RRT initialization completed.")
}

#' Refresh package - look for any new packages used and install those in rrt library
#' 
#' @export
#' @template rrt
rrt_refresh <- function(repo, verbose=TRUE)
{
  repoid <- digest(repo)
  if(is.null(repo)) repo <- getwd()
  
  # check to make sure repo exists
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    stop(sprintf("Repository %s doesn't exist", repo))
  }
  
  # check for rrt directory in the repo, and stop if it doesn't exist
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)[-1]
  if(!all(grepl("rrt", present))){
    stop("rrt directory doesn't exist")
  }
  
  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE)
  
  # install packages in a private location for this project
  mssg(verbose, "Installing new packages...")
  installPkgs(pkgs, lib, verbose)
  
  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repo, lib, pkgs, repoid)
  
  message("\n>>> RRT refresh completed.")
}

#' Function to install pkgs
#' 
#' @import miniCRAN
#' @export
#' @param x (character) A vector of package names. If NULL, none installed, and message prints
#' @param lib (character) Library location, a directory
#' @param recursive (logical) Recursively install packages?
#' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
#' @examples \dontrun{
#' installPkgs()
#' }
installPkgs <- function(x, lib, recursive=FALSE, verbose=TRUE){
  # check for existence of pkg, subset only those that need to be installed
  #     files <- list.files(lib, recursive = TRUE)
  pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
  if(!file.exists(pkgslist)) { pkgs2install <- x } else {
    installedpkgs <- gsub("Package:\\s", "", grep("Package:", readLines(pkgslist), value=TRUE))
    pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
  }
  if(!is.null(pkgs2install) || length(pkgs2install) == 0){  
    # FIXME, needs some fixes on miniCRAN to install source if binaries not avail.-This may be fixed now
    makeRepo(pkgs = pkgs2install, path = lib, download = TRUE)
  } else { 
    return(mssg(verbose, "No packages found - none installed"))
  }
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
#' # FIXME: check for existence of manifest file first, and combine with old info, if any
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
  sysreq <- sprintf("SystemRequirements: %s", paste0(rtt_compact(getsysreq(packs)), collapse = "\n") )
  pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ","))
  repositoryid <- sprintf("RepoID: %s", repoid)
  date <- sprintf("DateCreated: %s", Sys.time())
  info <- c(installedwith, installedfrom, rrtver, rver, date, path.expand(pkgsloc), repositoryid, pkgs_deps, sysreq)
  cat(info, file = infofile, sep = "\n")
}