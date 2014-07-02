#' Install packages within an RRT repository.
#'
#' @param repo A repository path. This is the path to the root of your RRT repository. By default,
#' we use the current working directory via \code{getwd()}.
#' @param repoid Repository ID.
#' @param lib Library path
#' @param verbose Print messages. Default: TRUE.

rrt_install2 <- function(repo=getwd(), repoid, lib, verbose=TRUE)
{
  pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
  
  mssg(verbose, "Looking for packages used in your repository...")
  x <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)
  
  if(!file.exists(pkgslist)) {
    mssg(verbose, "Getting new packages...")
    pkgs2install <- getPkgs(x, lib, verbose)
  } else {
    installedpkgs <- list.files(lib)
    installedpkgs <- installedpkgs[!installedpkgs %in% "src"]
    pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
  }
  
  basepkgs <- rownames(installed.packages(priority="base"))
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
      # alternative setup to install all pkgs in one install.packages call to avoid restart messages
      install.packages(pkgswithpath, lib = lib, repos=NULL, type = "source")
      notinst <- pkgs2install[!vapply(file.path(lib, pkgs2install), file.exists, logical(1))]
      if(!length(notinst) == 0) install.packages(notinst, lib = lib, destdir = file.path(lib, "src/contrib"))
#       try_install <- function(x){
#         pkgname <- strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]]
#         install.packages(x, lib = lib, repos=NULL, type = "source")
#         if(!file.exists(file.path(lib, pkgname))){
#           mssg(verbose, "Installation from source failed, trying binary package version...")
#           download.packages(pkgname, destdir = file.path(lib, "src/contrib"))
#           install.packages(pkgname, lib = lib)
#         }
#       }
#       lapply(pkgswithpath, try_install)
    }
  }
}