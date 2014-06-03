#' Get repository dependencies
#' 
#' This function first determines packages used in your repo, then recursively gets dependencies
#' of the packages you use (for Depends, Imports and LinkLibrary only, see \code{?pkgDep}). 
#'
#' @import miniCRAN packrat
#' @export
#' @param repo A path to a RRT repository; defaults to current working directory.
#' @return A named list of packages, named by the package that requires said dependencies
#' @examples
#' repodeps(repo="~/myrepo")
#' repodeps(repo="~/myrepo", simplify=TRUE)
#' setwd("~/myrepo")
#' repodeps()

repodeps <- function(repo=getwd(), simplify=FALSE)
{
  # Get packages used in the repo using appDependencies from packrat
  pkgs_used <- packrat:::appDependencies(repo)
  pkgs_used <- pkgs_used[!grepl("packrat", pkgs_used)]
  
  # Get package dependencies using miniCRAN
  pkg_deps <- lapply(pkgs_used, pkgDep)
  names(pkg_deps) <- pkgs_used
  
  if(simplify){
    allpkgs <- unname(do.call(c, pkg_deps))
    pkg_deps <- unique(allpkgs)
  }
  
  return(pkg_deps)
}