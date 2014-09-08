#' Install packages within an RRT repository.
#' 
#' @keywords internal
#'
#' @inheritParams checkpoint

#' @param srcPath (character) Location of package src in repo
#' @param libPath (character) Location of package library in repo
#' @param snapshotid (character) The identifying snapshot id, consisting of a date and semi-timestamp, e.g. "2014-08-01_0500"
#' 
#' @param suggests Install suggests or not. Default: FALSE.
#' @param verbose Print messages. Default: TRUE
#' @param quiet Passed to \code{\link[utils]{install.packages}}

rrt_install <- function(repo=getwd(), snapshotid,
                        libPath=rrtPath(repo, "lib"), 
                        srcPath=rrtPath(repo, "src"),
                        suggests=FALSE, 
                        verbose=TRUE, quiet=FALSE)
{
  # make src/contrib
  if(missing("snapshotid")) stop("missing snapshotid")
  createRepoFolders(repo)

  repo_pkgs <- scanRepoPackages(repo)
  if(length(repo_pkgs) == 0){
    mssg(verbose, "... nothing to install")
    return(NULL)
  }
  
#   repo_deps <- repoDependencies(repo_pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
  
  
  mssg(verbose, "Downloading packages used in your repository...")
  
  installed_pkgs <- list.files(libPath, recursive=FALSE, include.dirs=FALSE)
#   pkgs_to_install <- setdiff(repo_deps, installed_pkgs)
#   pkgs_to_install <- sort(pkgs_to_install)
  pkgs_to_install <- sort(setdiff(repo_pkgs, installed_pkgs))
  
#   cran_pkgs <- pkgs_to_install[is.cranPackage(pkgs_to_install)]
#   bioc_pkgs <- pkgs_to_install[is.biocPackage(pkgs_to_install)]
  cran_pkgs <- character(0)
  bioc_pkgs <- character(0)
  pkgsRemaining <- setdiff(pkgs_to_install, c(cran_pkgs, bioc_pkgs))
  
#   if(length(pkgsRemaining) > 0) {
#     download_github_pkgs(repo, pkgs = pkgsRemaining, libPath)
#     githubpkgs <- pkgsRemaining
#   } else { githubpkgs <- character(0) }

  
  downloadPackageFromMran(repo=repo, snapshotid = snapshotid,
                          srcPath=srcPath, 
                          pkgs=pkgsRemaining, 
                          verbose=verbose,
                          quiet=quiet)


  installRepoPackages(repo=repo, pkgs=pkgsRemaining, libPath=libPath, srcPath=srcPath, 
                      verbose=verbose, quiet=quiet)
  
  # check for any failed intalls and install from binary from default CRAN mirror
  installed <- vapply(repo_pkgs, FUN=function(p){ 
    file.exists(file.path(libPath, p)) 
    }, 
    FUN.VALUE=logical(1), USE.NAMES=TRUE
    )
  if(any(!installed)) {
#     mssg(verbose, "... Installing from default CRAN mirror")
#     utils::install.packages(notinst, lib = srcPath, destdir = srcPath, 
#                             quiet=quiet, verbose=verbose)
    stop("Some packages required but not found on MRAN")
  }
  
#   # install github pkgs
#   if(!length(githubpkgs) == 0) {
#     mssg(verbose, "... Installing from github")
#     install_github_pkgs(lib, githubpkgs)
#   }
  
#   # install (and download bioc pkgs)
#   if(!length(bioc_pkgs) == 0) {
#     mssg(verbose, "... Installing from BioConductor")
#     download_bioconductor_pkgs(lib, bioc_pkgs, repo)
#   }
  
}


# Installs downloaded repo packages from source.
# 
installRepoPackages <- function(repo, pkgs, 
                                libPath=rrtPath(repo, "lib"), 
                                srcPath=rrtPath(repo, "src"), 
                                verbose, quiet=FALSE){
  if(length(pkgs)==0){
    mssg(verbose, "... No MRAN packages found to install")
  } else {
    mssg(verbose, "Installing packages...")
    allPkgs <- list.files(srcPath, full.names = TRUE)
    names(allPkgs) <- gsub("_[0-9].+", "", list.files(srcPath))
    pkgsWithPath <- unname(sapply(pkgs, function(x) allPkgs[grepl(x, names(allPkgs))]))
    pkgsWithPath <- pkgsWithPath[!sapply(pkgsWithPath, length) == 0]
    
    if(length(pkgsWithPath) == 0){
      mssg(verbose, "... No MRAN packages found to install")
    } else {
      pkgsWithPath <- pkgsWithPath[!grepl("\\.zip", pkgsWithPath)]
      pkgsWithPath <- unlist(pkgsWithPath)
      pkgsWithPath <- normalizePath(pkgsWithPath)
      utils::install.packages(pkgsWithPath, lib = libPath, repos=NULL, 
                              type = "source", dependencies=FALSE, quiet=quiet)
    }
  }
}



# is.cranPackage <- function (pkgs, 
#                             repos = c(CRAN="http://cran.r-project.org/"), 
#                             type = "source"){
#   if (!grepl("^file", repos) && file.exists(repos)) {
#     repos <- paste0("file:///", repos)
#   }
#   tt <- available.packages(contrib.url(repos, type = type))
#   availcran_pkgs <- row.names(tt)
#   pkgs %in% availcran_pkgs
# }
# 
# 





