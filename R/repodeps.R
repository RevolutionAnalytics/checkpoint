#' Get repository dependencies
#' 
#' This function first determines packages used in the repo, then recursively gets dependencies
#' of the packages you use (for Depends, Imports and LinkLibrary only, see \code{?pkgDep}). 
#' Optionally, you can get dependencies for Suggests and Enhances (non-recursively). NOTE: 
#' Enhances not working right now.
#'
#' @import miniCRAN yaml
#' @export
#' 
#' @param repo (character) A path to a RRT repository; defaults to current working directory.
#' @param simplify (logical) If TRUE, simplify list to a vector with all unique packages.
#' @param base (logical) If TRUE, return base R packages, if FALSE, don't return them.
#' @param ... Further args passed on to \code{miniCRAN::pkgDep}
#' 
#' @keywords internal
#' @return A named list of packages, named by the package that requires said dependencies
#' @examples \dontrun{
#' repodeps(repo="~/newrepo")
#' repodeps(repo="~/newrepo", simplify=TRUE)
#' 
#' # defaults to working directory
#' setwd("~/newrepo")
#' repodeps() 
#' 
#' # Get Suggests too, not retrieved by default
#' repodeps(repo="~/newrepo", simplify=TRUE, suggests=TRUE)
#' }

repodeps <- function(repo=getwd(), simplify=FALSE, base=TRUE, ...)
{
  # Get packages used in the repository
  pkgs_used <- rrt_deps(repo)
  # remove RRT
  pkgs_used <- pkgs_used[!pkgs_used %in% 'RRT']
  
  # Get package dependencies using miniCRAN
  pkg_deps <- lapply(pkgs_used, pkgDep, ...)
  names(pkg_deps) <- pkgs_used
  
  if(simplify){
    allpkgs <- unname(do.call(c, pkg_deps))
    pkg_deps <- unique(allpkgs)
  }
  
  # remove base R pkgs
  if(!base){
    availpkgs <- available.packages(contrib.url(getOption("repos"), "source"))
    pkg_deps <- pkg_deps[!sapply(pkg_deps, function(x){
      zz <- tryCatch(availpkgs[x,]["Priority"], error=function(e) e)
      if("error" %in% class(zz)) { FALSE } else {
        if(!is.na(zz[['Priority']]) && zz[['Priority']] == "base") TRUE else FALSE
      }
    })]
    pkg_deps <- pkg_deps[!pkg_deps %in% c('stats','utils','grDevices','graphics','methods','grid')]
  }
  
  return(pkg_deps)
}

pkgDep_try <- function(x, ...){
  tmp <- tryCatch(pkgDep(x, ...), error=function(e) e)
  if(!"error" %in% class(tmp)){ tmp } else {
    pkg_deps_noncran(x)
  }
}

pkg_deps_noncran <- function(repo, x){
  ### FIXME: if we index non-CRAN packages on MRAN, we could easily search MRAN instead of the mess below
  # look for package mention in manifest, return message if not
  manfile <- file.path(repo, "rrt/rrt_manifest.yml")
  if(!file.exists(manfile)){ tt <- "not found" } else {  
    tt <- yaml.load_file(manfile)
    nn <- grep("Github|MRAN|Bitbucket|Bioconductor|Gitorious", tt, value = TRUE)
    agrep(x, nn)
  }
  if(){ 
    
  } else {
    sprintf("%s not found - make sure to specify info in the manifest file at %s", x, manfile)
  }
}