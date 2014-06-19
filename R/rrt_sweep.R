#' Clean out installed packages in your repository
#' 
#' @import digest
#' @export
#' @param repo Repository path
#' @param verbose (logical) Print messages (default) or not
#' @examples \dontrun{
#' rrt_sweep(repo="~/testrepo")
#' }

rrt_sweep <- function(repo, verbose=TRUE)
{  
  # setup
  ## create repo id using digest
  repoid <- digest(repo)
  if(is.null(repo)) repo <- getwd()
  
  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }
  
  # check for rrt directory in the repo
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(lib)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }
  
  # get pkgs list in the rrt repo
  pathtoremove <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  dirstoremove <- list.files(pathtoremove, full.names = TRUE, recursive = FALSE)
  srctoremove <- grep("src", dirstoremove, value = TRUE)
  pkgnames <- list.files(pathtoremove, recursive = FALSE)
  pkgnames <- pkgnames[!pkgnames %in% "src"]
  
  # remove all packages and source tar balls
  ## remove src (source files)
  unlink(srctoremove, recursive = TRUE)
  ## remove installed packages
  remove.packages(pkgnames, pathtoremove)

  cat("Your repository packages successfully removed:", pkgnames, sep = "\n")
}