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
#' @param pkg Package name
#' @param date MRAN snapshot date
#' @param lib Library to install packages in
#' @param destdir Destination directory to install packages to
#' @param ... Further args passed on to \code{\link[utils]{install.packages}}
#' @param dependencies Whether to install dependencies or not. Default: TRUE. See 
#' @param quiet Passed to \code{\link[utils]{install.packages}}
#' \link{install.packages}
#'
#' @return Installs a package, or throws warnings/errors on failures
#' @family mran
#'
#' @examples \dontrun{
#' install_mran(pkg="plyr", date="2014-08-01", lib="~/mran_snaps/")
#' install_mran(pkg="calmate", date="2014-08-01", lib="~/mran_snaps/")
#' }
install_mran <- function(pkg, date=NULL, lib = NULL, destdir = NULL, quiet=FALSE, ..., dependencies = TRUE)
{
  pkgs_mran(date=date, pkgs=pkg, outdir=lib, quiet=quiet)
  files <- list.files(lib, pattern = ".tar.gz")
  files <- grep(pkg, files, value = TRUE)
  sapply(files, install_mran_single, ..., lib=lib, destdir=destdir, dependencies=dependencies, quiet=quiet)
}

install_mran_single <- function(pkg, date=NULL, lib = NULL, destdir = NULL, quiet=FALSE, ..., dependencies = TRUE){
  path <- file.path(lib, pkg)
  pkgname <- strsplit(strsplit(pkg, "/")[[1]][ length(strsplit(pkg, "/")[[1]]) ], "_")[[1]][[1]]
  tmpdir <- tempdir()
  untar(path, exdir = tmpdir)
  path2 <- file.path(tmpdir, pkgname)
  info <- pkg_deps(path2)
  download_deps(pkg = path2, info=info, lib=lib, dependencies = dependencies, quiet=quiet)
  depscheck <- Map(needs_install, info$name, info$compare, info$version, lib)
  depsinstall <- info$name[as.logical(depscheck)]
  just_deps <- sapply(depsinstall, function(x) grep(x, list.files(lib, pattern = ".tar.gz"), value = TRUE))
  just_deps_paths <- file.path(lib, just_deps)
  
  # install dependencies
  utils::install.packages(just_deps_paths, ..., lib=lib, dependencies=TRUE, repos=NULL, type = "source", quiet=quiet)
  
  # install target package
  utils::install.packages(pkgs=file.path(lib, grep(pkgname, list.files(lib, pattern = ".tar.gz"), value = TRUE)), 
                   lib=lib, dependencies=FALSE, repos=NULL, type = "source", quiet=quiet, ...)
}

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

download_deps <- function (pkg = NULL, info, lib, dependencies = NA, quiet=FALSE)
{
  pkg <- as.package(pkg)
#   info <- pkg_deps(pkg, dependencies)
  needed <- Map(needs_install, info$name, info$compare, info$version, lib=lib)
  deps <- info$name[as.logical(needed)]
  if (length(deps) == 0) return(invisible())
  message("Downloading dependencies for ", pkg$package, ":\n", paste(deps, collapse = ", "))
  pkgs_mran(date=date, pkgs=deps, outdir=lib, quiet=quiet)
  invisible(deps)
}

needs_install <- function(pkg, compare, version, lib) {
  if (length(find.package(pkg, lib.loc=lib, quiet = TRUE)) == 0) return(TRUE)
  if (is.na(compare)) return(FALSE)
  compare <- match.fun(compare)
  !compare(packageVersion(pkg, lib.loc=lib), version)
}

