#' Install packages within an RRT repository.
#' 
#' @import BiocInstaller
#' @keywords internal
#'
#' @param repo A repository path. This is the path to the root of your RRT repository. By default, we use the current working directory via \code{\link{getwd}}.
#' @param lib Library path
#' @param suggests Install suggests or not. Default: FALSE.
#' @param verbose Print messages. Default: TRUE
#' @param quiet Passed to \code{\link[utils]{install.packages}}

rrt_install <- function(repo=getwd(), snapshot,
                        libPath=rrtPath(repo, "lib"), 
                        srcPath=rrtPath(repo, "src"),
                        suggests=FALSE, 
                        verbose=TRUE, quiet=FALSE)
{
  # make src/contrib
  if(missing("snapshot")) stop("missing snapshot")
  createRepoFolders(repo)

  repo_pkgs <- scanRepoPackages(repo)
  if(length(repo_pkgs) == 0){
    mssg(verbose, "... nothing to install")
    return(NULL)
  }
  
  repo_deps <- repoDependencies(repo_pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
  
  
  mssg(verbose, "Downloading packages used in your repository...")
  
  installed_pkgs <- list.files(libPath, recursive=FALSE, include.dirs=FALSE)
  pkgs_to_install <- setdiff(repo_deps, installed_pkgs)
  pkgs_to_install <- sort(pkgs_to_install)
  
  cran_pkgs <- pkgs_to_install[is.cranPackage(pkgs_to_install)]
#   bioc_pkgs <- pkgs_to_install[is.biocPackage(pkgs_to_install)]
  bioc_pkgs <- character(0)
  pkgsRemaining <- setdiff(pkgs_to_install, c(cran_pkgs, bioc_pkgs))
  
#   if(length(pkgsRemaining) > 0) {
#     download_github_pkgs(repo, pkgs = pkgsRemaining, libPath)
#     githubpkgs <- pkgsRemaining
#   } else { githubpkgs <- character(0) }
  
  downloadPackageFromMran(repo=repo, libPath=libPath, date=NULL,
                     snapshot = snapshot, 
                     pkgs=cran_pkgs, 
                     srcPath=srcPath, 
                     verbose=verbose,
                     quiet=quiet)

  installRepoPackages(libPath, cran_pkgs, verbose=verbose, quiet=quiet)
  
  # check for any failed intalls and install from binary from default CRAN mirror
  notinst <- cran_pkgs[!vapply(file.path(srcPath, cran_pkgs), file.exists, logical(1))]
  if(!length(notinst) == 0) {
    mssg(verbose, "... Installing from default CRAN mirror")
    utils::install.packages(notinst, lib = srcPath, destdir = srcPath, 
                            quiet=quiet, verbose=verbose)
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


#' Installs downloaded repo packages from source.
#' 
installRepoPackages <- function(repo, 
                                libPath=rrtPath(repo, "lib"), 
                                srcPath=rrtPath(repo, "src"), 
                                pkgs, verbose, quiet=FALSE){
  if(length(yyy)==0){
    mssg(verbose, "... No MRAN packages found to install")
  } else {
    mssg(verbose, "Installing packages...")
    allPkgs <- list.files(srcPath, full.names = TRUE)
    names(allPkgs) <- gsub("_[0-9].+", "", list.files(srcPath))
    allPkgs <- setdiff(allPkgs, "PACKAGES")
    pkgsWithPath <- unname(sapply(pkgs, function(x) allPkgs[grepl(x, names(allPkgs))]))
    pkgsWithPath <- pkgsWithPath[!sapply(pkgsWithPath, length) == 0]
    
    if(length(pkgsWithPath) == 0){
      mssg(verbose, "... No MRAN packages found to install")
    } else {
      pkgsWithPath <- pkgsWithPath[!grepl("\\.zip", pkgsWithPath)]
      pkgsWithPath <- unlist(pkgsWithPath)
      pkgsWithPath <- normalizePath(pkgsWithPath, winslash="/")
      utils::install.packages(basename(pkgsWithPath), lib = libPath, repos=NULL, 
                              type = "source", dependencies=FALSE, quiet=quiet)
    }
  }
}



is.cranPackage <- function (pkgs, 
                            repos = c(CRAN="http://cran.r-project.org/"), 
                            type = "source"){
  if (!grepl("^file", repos) && file.exists(repos)) {
    repos <- paste0("file:///", repos)
  }
  tt <- available.packages(contrib.url(repos, type = type))
  availcran_pkgs <- row.names(tt)
  pkgs %in% availcran_pkgs
}







