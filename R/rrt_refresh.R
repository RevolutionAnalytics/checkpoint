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
#' @family rrt
#' @seealso \code{\link{rrt_init}}, \code{\link{rrt_install}}
#'

rrt_refresh <- function(repo=getwd(), mran=TRUE, snapdate=NULL, autosnap=TRUE, verbose=TRUE,
                        suggests=FALSE, quiet=FALSE)
{
  repoid <- repoDigest(repo)

  # check to make sure repo exists
  if(!repo.exists(repo)) create_repo_folder(repo)
  
  # check for rrt directory in the repo, and stop if it doesn't exist
  libPath <- rrtPath(repo, "lib")
  srcPath <- rrtPath(repo, type = "src")
  createRepoFolders(repo)
  
  # get packages in a private location for this project
  setSnapshotInOptions(repo=repo, snapdate=snapdate, autosnap=autosnap)

  # Write blank user manifest file
  writeUserManifest(repo = repo, verbose = verbose)
  
  # download and install packages
  rrt_install(repo, repoid=repoid, libPath=libPath, mran=mran, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  pkgs <- scanRepoPackages(repo)
#   pkgs <- repoDependencies(pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
  writeManifest(repository = repo, libPath = libPath, packs = pkgs, snapshot = getOption('RRT_snapshotID'), repoid)

  # write package versions to manifest file
  write_pkg_versions(repo, srcPath)

  # Write repo log file
  rrt_repos_write(repo, repoid)

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  message("\n>>> RRT refresh completed.")
}
