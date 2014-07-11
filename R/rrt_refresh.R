#' Refresh package - look for any new packages used and install those in rrt library
#'
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://mran.revolutionanalytics.com} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param autosnap (logical) Get most recent snapshot. Default: FALSE
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param suggests (logical) Download and install packages in the Suggests line for packages
#' used in your RRT repository, or not. Default: FALSE.
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

rrt_refresh <- function(repo=getwd(), mran=TRUE, snapdate=NULL, autosnap=FALSE, verbose=TRUE, 
                        suggests=FALSE)
{
  repoid <- digest(normalizePath(repo))

  # check to make sure repo exists
  check4repo(repo, verbose)

  # check for rrt directory in the repo, and stop if it doesn't exist
  lib <- rrt_libpath(repo)
  check4rrt(repo, lib, verbose)

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)
  ## remove packages not found on MRAN
  if(!is.null(pkgs)){ 
    notfound <- pkgs[grepl("not found", pkgs)]
    if(!length(notfound) == 0){
      cat(notfound, sep = "\n")
    }
    pkgs <- pkgs[!grepl("not found", pkgs)]
  }

  # Look for packages installed by user but no source available
  # if some installed give back vector of package names
  addtnpkgs <- checkuserinstall(lib)
  pkgs <- c(addtnpkgs, pkgs)

  # get packages in a private location for this project
  mssg(verbose, "Getting new packages...")
  if(autosnap){
    availsnaps <- suppressMessages(mran_snaps())
    snapshotid <- availsnaps[length(availsnaps)]
  } else { snapshotid <- NULL }
  getPkgs(x = pkgs, repo = repo, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate, snapshotid = snapshotid)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repository = repo, librar = lib, packs = pkgs, repoid)

  # Write repo log file
  rrt_repos_write(repo, repoid)

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  # install packages
  rrt_install2(repo, repoid, lib, suggests, verbose)
  
  # write package versions to manifest file
  write_pkg_versions(lib, repo)

  message("\n>>> RRT refresh completed.")
}
