#' Refresh package - look for any new packages used and install those in rrt library
#'
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#'
#' @seealso \link{rrt_init}, \link{rrt_install}
#'
#' @examples \dontrun{
#' rrt_init(repo="~/testrepo")
#' rrt_refresh(repo="~/testrepo")
#' rrt_refresh(repo="~/testrepo", mran=TRUE)
#' rrt_install(repo="~/testrepo")
#'
#' # Optionally, do an interactive repo intitialization
#' rrt_init(repo="~/mynewcoolrepo", interactive=TRUE)
#' }

rrt_refresh <- function(repo=getwd(), mran=TRUE, snapdate=NULL, verbose=TRUE)
{
  repoid <- digest(repo)

  # check to make sure repo exists
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    stop(sprintf("Repository %s doesn't exist", repo))
  }

  # check for rrt directory in the repo, and stop if it doesn't exist
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, base::getRversion())
  if(!file.exists(file.path(repo, "rrt"))){
    mssg(verbose, sprintf("Creating rrt directory %s", lib))
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  } else { mssg(verbose, "rrt directory already exists") }

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)

  # Look for packages installed by user but no source available
  # if some installed give back vector of package names
  addtnpkgs <- checkuserinstall(lib)
  pkgs <- c(addtnpkgs, pkgs)
  
  # get packages in a private location for this project
  mssg(verbose, "Getting new packages...")
  getPkgs(x = pkgs, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repository = repo, librar = lib, packs = pkgs, repoid)
  
  # Write repo log file
  rrt_repos_write(repo, repoid)
  
  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  message("\n>>> RRT refresh completed.")
}
