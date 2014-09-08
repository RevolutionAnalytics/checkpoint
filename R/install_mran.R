#' Install packages from MRAN
#'
#' Steps (for each pkg):
#'  download package
#'  determine dependencies
#'  download dependencies (but don't install them)
#'  install target pkg and downloaded dependencies
#'
#' @import devtools
#' @export
#' 
#' @inheritParams checkpoint
#' @inheritParams rrt_init
#' @inheritParams rrt_install
#' 
#'
#' @param pkg Package name
#' @param snapshot MRAN snapshot id
#' @param ... Further args passed on to \code{\link[utils]{install.packages}}
#' @param dependencies Whether to install dependencies or not. Default: TRUE. See 
#'
#' @return Installs a package, or throws warnings/errors on failures
#' @family mran
#'
#' @example \inst\examples\example_install_mran.R 
install_mran <- function(pkg, repo, snapshotdate=NULL, 
                         libPath = rrtPath(repo, "lib"), 
                         srcPath = rrtPath(repo, "src"), 
                         snapshot=getSnapshotId(snapshotdate),
                         quiet=FALSE, ..., dependencies = TRUE)
{
  downloadPackageFromMran(pkgs=pkg, snapshot=snapshot, srcPath=srcPath, quiet=quiet)
  files <- list.files(srcPath, pattern = ".tar.gz")
  files <- grep(pkg, files, value = TRUE)
  sapply(files, install_mran_single, ..., libPath=libPath, srcPath=srcPath, 
         dependencies=dependencies, quiet=quiet)
}



install_mran_single <- function(pkg, repo, snapshotdate=NULL, 
                                libPath = rrtPath(repo, "lib"), 
                                srcPath = rrtPath(repo, "src"), 
                                quiet=FALSE, ..., dependencies = TRUE){
  browser()
  path <- file.path(srcPath, pkg)
  pkgname <- gsub("(.*?)_.*", "\\1", pkg)
  tmpdir <- tempdir()
  untar(path, exdir = tmpdir)
  downloadedPkg <- file.path(tmpdir, pkgname)
  info <- pkg_deps(downloadedPkg)
  download_deps(pkg = downloadedPkg, info=info, srcPath=srcPath, dependencies = dependencies, quiet=quiet)
  depscheck <- Map(needs_install, info$name, info$compare, info$version, libPath)
  depsinstall <- info$name[as.logical(depscheck)]
  just_deps <- sapply(depsinstall, function(x) {
    grep(x, list.files(srcPath, pattern = ".tar.gz"), value = TRUE)
  })
  just_deps_paths <- file.path(srcPath, just_deps)
  
  # install dependencies
  utils::install.packages(just_deps_paths, ..., lib=libPath, dependencies=TRUE, 
                          repos=NULL, type = "source", quiet=quiet)
  
  # install target package
  pkgs <- file.path(srcPath, grep(pkgname, list.files(srcPath, pattern = ".tar.gz"), value = TRUE))
  utils::install.packages(pkgs, lib=libPath, dependencies=FALSE, 
                          repos=NULL, type = "source", quiet=quiet, ...)
}




#' Determine packages dependencies.
#'  
#' @keywords Internal
#' @param pkg package description, can be path or package name. See \code{\link[devtools]{as.package}} for more information
#' @param dependencies logical indicating to also install uninstalled packages which this \code{pkg} depends on/links to/suggests. See argument \code{dependencies} of \code{\link[utils]{install.packages}}.
#' @note This function copied from devtools:::pkg_deps
#' 
pkg_deps <- function (pkg = ".", dependencies = NA){
  pkg <- as.package(pkg)
  deps <- if (identical(dependencies, NA)) {
    c("Depends", "Imports", "LinkingTo")
  } else if (isTRUE(dependencies)) {
    c("Depends", "Imports", "LinkingTo", "Suggests", "VignetteBuilder")
  } else if (identical(dependencies, FALSE)) {
    character(0)
  } else dependencies
  deps <- unlist(pkg[tolower(deps)], use.names = FALSE)
  parse_deps(paste(deps, collapse = ","))
}



download_deps <- function (pkg = NULL, info, srcPath, dependencies = NA, quiet=FALSE, snapshot){
  if(missing("snapshot")) stop("snapshot missing")
  pkg <- as.package(pkg)
  needed <- Map(needs_install, info$name, info$compare, info$version, lib=srcPath)
  deps <- info$name[as.logical(needed)]
  if (length(deps) == 0) return(invisible())
  message("Downloading dependencies for ", pkg$package, ":\n", paste(deps, collapse = ", "))
  downloadPackageFromMran(pkgs=deps, snapshot=snapshot, srcPath=srcPath, quiet=quiet)
  invisible(deps)
}



needs_install <- function(pkg, compare, version, lib) {
  if (length(find.package(pkg, lib.loc=lib, quiet = TRUE)) == 0) return(TRUE)
  if (is.na(compare)) return(FALSE)
  compare <- match.fun(compare)
  !compare(packageVersion(pkg, lib.loc=lib), version)
}

