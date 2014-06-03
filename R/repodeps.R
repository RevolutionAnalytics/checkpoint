#' Get repository dependencies
#' 
#' This function first determines packages used in the repo, then recursively gets dependencies
#' of the packages you use (for Depends, Imports and LinkLibrary only, see \code{?pkgDep}). 
#' Optionally, you can get dependencies for Suggests and Enhances (non-recursively). NOTE: 
#' Enhances not working right now.
#'
#' @import miniCRAN packrat
#' @export
#' 
#' @param repo (character) A path to a RRT repository; defaults to current working directory.
#' @param simplify (logical) If TRUE, simplify list to a vector with all unique packages.
#' 
#' @return A named list of packages, named by the package that requires said dependencies
#' @examples \dontrun{
#' repodeps(repo="~/myrepo")
#' repodeps(repo="~/myrepo", simplify=TRUE)
#' setwd("~/myrepo")
#' repodeps()
#' repodeps(repo="~/myrepo", simplify=TRUE, enhances=TRUE)
#' }

repodeps <- function(repo=getwd(), simplify=FALSE, ...)
{
  # Get packages used in the repo using appDependencies from packrat
  ## Note, appDependencies not exported from packrat, so ::: needed, probably a different solution later
  pkgs_used <- packrat:::appDependencies(repo)
  pkgs_used <- pkgs_used[!grepl("packrat", pkgs_used)]
  
  # Get package dependencies using miniCRAN
  pkg_deps <- lapply(pkgs_used, pkgDep, ...)
  names(pkg_deps) <- pkgs_used
  
  if(simplify){
    allpkgs <- unname(do.call(c, pkg_deps))
    pkg_deps <- unique(allpkgs)
  }
  
  return(pkg_deps)
}
