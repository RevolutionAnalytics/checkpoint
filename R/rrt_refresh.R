#' Refresh package - look for any new packages used and install those in rrt library.
#'
#' @export
#' 
#' @inheritParams checkpoint
#' @inheritParams rrt_init
#'
#' @family rrt
#' @seealso \code{\link{rrt_init}}, \code{\link{rrt_install}}
#'

rrt_refresh <- function(repo=getwd(), mran=TRUE, snapshotdate=NULL, autosnap=TRUE, verbose=TRUE,
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
  setSnapshotInOptions(repo=repo, snapshotdate=snapshotdate, autosnap=autosnap)

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
