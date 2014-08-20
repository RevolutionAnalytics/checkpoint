#' Refresh package - look for any new packages used and install those in rrt library.
#'
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://mran.revolutionanalytics.com} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param autosnap (logical) Get most recent snapshot. Default: FALSE
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param suggests (logical) Download and install packages in the Suggests line for packages used in your RRT repository, or not. Default: FALSE.
#' @param quiet Passed to \code{\link[utils]{install.packages}}
#'
#' @seealso \code{\link{rrt_init}}, \code{\link{rrt_install}}
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
                        suggests=FALSE, quiet=FALSE)
{
  repoid <- digest(suppressWarnings(normalizePath(repo)))

  # check to make sure repo exists
  check4repo(repo, verbose)

  # check for rrt directory in the repo, and stop if it doesn't exist
  lib <- rrt_libpath(repo)
  check4rrt(repo, lib, verbose)

  # get packages in a private location for this project
  set_snapshot_date(repo, snapdate, autosnap)

  # Write blank user manifest file
  writeUserManifest(repository = repo, verbose = verbose)
  
  # download and install packages
  rrt_install(repo, repoid=repoid, lib=lib, mran=mran, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)
  writeManifest(repository = repo, librar = lib, packs = pkgs, snapshot = getOption('RRT_snapshotID'), repoid)

  # write package versions to manifest file
  write_pkg_versions(lib, repo)

  # Write repo log file
  rrt_repos_write(repo, repoid)

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  message("\n>>> RRT refresh completed.")
}
