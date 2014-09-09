#' Clean out installed packages and their sources in your repository
#'
#' @export
#' @param repo Repository path, Defaults to the current working directory
#' @param pkgs A vector of package names
#' @param verbose (logical) Print messages (default) or not
#' @family rrt
#' @examples \dontrun{
#' rrt_refresh(repo="~/testrepo")
#' rrt_sweep(repo="~/testrepo")
#' }

rrt_sweep <- function(repo=getwd(), pkgs = NULL, verbose=TRUE)
{
  ## create repo id
  repoid <- repoDigest(repo)

  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }

  # check for rrt directory in the repo
  mssg(verbose, "Checking to make sure rrt directory exists inside your repository...")
  lib <- rrtPath(repo, "lib")
  present <- list.dirs(lib)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }

  # get pkgs list in the rrt repo
  pathToRemove <- rrtPath(repo, "lib")
  dirsToRemove <- list.files(pathToRemove, full.names = TRUE, recursive = FALSE)
  srcToRemove <- grep("src", dirsToRemove, value = TRUE)
  if(is.null(pkgs)){
    pkgNames <- list.files(pathToRemove, recursive = FALSE)
    pkgNames <- pkgNames[!pkgNames %in% "src"]
  } else { pkgNames <- pkgs }

  ## remove src (source files)
  unlink(srcToRemove, recursive = TRUE)
  mssg(verbose, paste("Package sources removed: ", sep = "\n"))

  if(length(pkgNames) == 0){ cat("No installed packages to remove :)") } else {
    ## remove installed packages
    remove.packages(pkgNames, pathToRemove)

    mssg(verbose, paste("Your repository packages successfully removed:", pkgNames, sep = "\n"))
  }
}
