#' Install packages
#'
#' Installs packages to your local repository. Performs some checks to make sure your repo is a
#' RRT repository, and the rrt dir exists with your repository, then installs packages not already
#' installed.
#'
#' @export
#' @param repo A repository path
#' @param verbose Print messages
#'
#' @seealso \link{rrt_init}, \link{rrt_refresh}
#'
#' @examples \dontrun{
#' rrt_install(repo="~/testrepo")
#' }

rrt_install <- function(repo=getwd(), verbose=TRUE)
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
  present <- list.dirs(repo)[-1]
  ## ignore git dir
  present <- present[!grepl("\\.git", present)]
  if(!all(grepl("rrt", present))){
    stop("rrt directory doesn't exist")
  }

  pkgslist <- paste0(lib, "/src/contrib/PACKAGES")

  mssg(verbose, "Looking for packages used in your repository...")
  x <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)

  if(!file.exists(pkgslist)) {
    mssg(verbose, "Getting new packages...")
    pkgs2install <- getPkgs(x, lib, verbose)
  } else {
    #     installedpkgs <- gsub("Package:\\s", "", grep("Package:", readLines(pkgslist), value=TRUE))
    installedpkgs <- list.files(lib)
    installedpkgs <- installedpkgs[!installedpkgs %in% "src"]
    pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
  }

  basepkgs <- c('tools','methods','utils','stats')
  pkgs2install <- pkgs2install[!pkgs2install %in% basepkgs]

  if(length(pkgs2install)==0){
    mssg(verbose, "No packages found to install")
  } else {
    ## FIXME, check if already installed
    mssg(verbose, "Installing packages...")
    allpkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
    names(allpkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
    allpkgs <- allpkgs[!grepl("PACKAGES", allpkgs)]
    pkgswithpath <- unname(sapply(pkgs2install, function(x) allpkgs[grepl(x, names(allpkgs))]))
    pkgswithpath <- pkgswithpath[!sapply(pkgswithpath, length) == 0]
    if(length(pkgswithpath) == 0){
      mssg(verbose, "No packages found to install")
    } else {
      pkgswithpath <- unlist(pkgswithpath)
      try_install <- function(x){
        pkgname <- strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]]
        #         installfrom <- file.path(lib, "src/contrib")
        #         install.packages(x, lib = lib, repos=NULL, type = "source")
        install.packages(x, lib = lib, repos=NULL, type = "source")
        if(!file.exists(file.path(lib, pkgname))){
          mssg(verbose, "Installation from source failed, trying binary package version...")
          download.packages(pkgname, destdir = file.path(lib, "src/contrib"))
          install.packages(pkgname, lib = lib)
        }
      }
      lapply(pkgswithpath, try_install)
    }
  }
}
