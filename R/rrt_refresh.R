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

rrt_refresh <- function(repo, snapshotdate=NULL, mran=TRUE, autosnap=TRUE, verbose=TRUE,
                        suggests=FALSE, quiet=FALSE)
{
  if(missing("repo") || is.null(repo)) stop("repo not specified")
  repoid <- repoDigest(repo)
  createRepoFolders(repo)
  
  # check for rrt directory in the repo, and stop if it doesn't exist
  libPath <- rrtPath(repo, "lib")
  srcPath <- rrtPath(repo, type = "src")
  
  # get packages in a private location for this project
  snapshotid <- setSnapshotInOptions(repo=repo, snapshotdate=snapshotdate, autosnap=autosnap)

  # Write blank user manifest file
  writeUserManifest(repo = repo, verbose = verbose)
  
  # download and install packages
  rrt_install(repo, snapshotid=snapshotid, libPath=libPath, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  pkgs <- scanRepoPackages(repo)
  # pkgs <- repoDependencies(pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
  writeManifest(repo = repo, libPath = libPath, pkgs = pkgs, snapshot = getOption('RRT_snapshotID'), repoid)

  # write package versions to manifest file
  writePackagesToManifest(repo, libPath)

  # Write repo log file
  rrt_repos_write(repo, repoid)

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  message("\n>>> RRT refresh completed.")
}
