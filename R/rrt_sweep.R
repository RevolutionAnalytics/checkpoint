#' Clean out installed packages and their sources in your repository
#'
#' @import digest
#' @export
#' @param repo Repository path, Defaults to the current working directory
#' @param pkgs A vector of package names
#' @param verbose (logical) Print messages (default) or not
#' @examples \dontrun{
#' rrt_refresh(repo="~/testrepo")
#' rrt_sweep(repo="~/testrepo")
#' }

rrt_sweep <- function(repo=getwd(), pkgs = NULL, verbose=TRUE)
{
  ## create repo id using digest
  repoid <- digest(normalizePath(repo))

  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }

  # check for rrt directory in the repo
  mssg(verbose, "Checking to make sure rrt directory exists inside your repository...")
  lib <- rrt_libpath(repo)
  present <- list.dirs(lib)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }

  # get pkgs list in the rrt repo
  pathtoremove <- rrt_libpath(repo)
  dirstoremove <- list.files(pathtoremove, full.names = TRUE, recursive = FALSE)
  srctoremove <- grep("src", dirstoremove, value = TRUE)
  if(is.null(pkgs)){
    pkgnames <- list.files(pathtoremove, recursive = FALSE)
    pkgnames <- pkgnames[!pkgnames %in% "src"]
  } else { pkgnames <- pkgs }

  ## remove src (source files)
  unlink(srctoremove, recursive = TRUE)
  cat("Package sources removed", sep = "\n")

  if(length(pkgnames) == 0){ cat("No installed packages to remove :)") } else {
    ## remove installed packages
    remove.packages(pkgnames, pathtoremove)

    cat("Your repository packages successfully removed:", pkgnames, sep = "\n")
  }
}
