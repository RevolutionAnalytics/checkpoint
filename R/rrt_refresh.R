#' Refresh package - look for any new packages used and install those in rrt library
#'
#' @export
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See 
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
rrt_refresh <- function(repo=getwd(), mran=FALSE, snapdate=NULL, verbose=TRUE)
{
  repoid <- digest(repo)
  
  # check to make sure repo exists
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    stop(sprintf("Repository %s doesn't exist", repo))
  }
  
  # check for rrt directory in the repo, and stop if it doesn't exist
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)[-1]
  ## ignore git dir
  present <- present[!grepl(".git", present)]
  if(!all(grepl("rrt", present))){
    stop("rrt directory doesn't exist")
  }
  
  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)
  
  # get packages in a private location for this project
  mssg(verbose, "Getting new packages...")
  getPkgs(x = pkgs, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate)
  
  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repo, lib, pkgs, repoid)
  
  message("\n>>> RRT refresh completed.")
}