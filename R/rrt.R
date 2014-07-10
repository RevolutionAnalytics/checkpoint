#' Initiate or refresh an RRT repository.
#'
#' This function decides what to do with your repository based on commands you give, and on
#' skimming through your repo. You can run this function to start a new repository,
#' without any work done yet, creating a new folder and RRT files, or you can initiate a RRT
#' repository inside an existing project/folder you already have. If the latter, we don't alter
#' your files at all, but simply write a few files needed for RRT to work properly. By detault
#' repo initialization is done interactively, so that you can choose your settings or accept
#' reasonable defaults.
#' 
#' You can run this function to start a new RRT repo, and to refresh a repo with new work. This 
#' function downloads packages and installs them, as needed.
#'
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://mran.revolutionanalytics.com} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param autosnap (logical) Get most recent snapshot. Default: FALSE
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo.
#' @param interactive (logical) If TRUE, function asks you for input for each item,
#' otherwise, defaults are used. Default: FALSE.
#' @param suggests (logical) Download and install packages in the Suggests line for packages
#' used in your RRT repository, or not. Default: FALSE.
#'
#' @seealso \link{rrt_refresh}, \link{rrt_init}
#'
#' @return Files written to the user's machine, with informative messages on progress
#'
#' @examples \dontrun{
#' # new repo
#' rrt(repo="~/scottsnewrepo")
#' 
#' # refresh repo
#' rrt(repo="~/scottsnewrepo")
#' }

rrt <- function(repo=getwd(), mran=TRUE, snapdate=NULL, autosnap=FALSE, verbose=TRUE, 
                     rprofile=NULL, interactive=FALSE, suggests=FALSE)
{
  if(!has_rsync()) mran <- FALSE
  
  if(is_rrt(repo, verbose = FALSE)){
    mssg(verbose, "RRT repo recognized...refreshing...\n")
    rrt_refresh(repo=repo, mran=mran, snapdate=snapdate, autosnap=autosnap, verbose=verbose, 
                suggests=suggests) 
  } else {
    mssg(verbose, "Creating a new RRT repo...\n")
    rrt_init(repo=repo, mran=mran, snapdate=snapdate, autosnap=autosnap, verbose=verbose, 
             rprofile=rprofile, interactive=interactive, suggests=suggests)
  }
}

has_rsync <- function(){
  x <- Sys.which("rsync")
  if(grepl("rsync", x)) TRUE else FALSE
}