#' Install packages from MRAN
#' 
#' Steps (for each pkg):
#'  download package
#'  determine dependencies
#'  download dependencies (but don't install them)
#'  install target pkg and downloaded dependencies
#' 
#' @import devtools plyr
#' @export
#' 
#' @param pkg Package name
#' @param snapshot MRAN snapshot ID
#' @param ... Further args passed on to \link{install.packages}
#' 
#' @return Installs a package, or throws warnings/errors on failures
#' 
#' @examples \dontrun{
#' install_mran(pkg="plyr", date="2014-06-23", lib="~/mran_snaps/")
#' install_mran(pkg="calmate", date="2014-06-23", lib="~/mran_snaps/")
#' }

install_mran <- function(pkg, date=NULL, lib = NULL, destdir = NULL, ..., dependencies = TRUE)
{
  pkgs_mran(date=date, pkgs=pkg, outdir=lib)
  files <- list.files(lib, pattern = ".tar.gz")
  files <- grep(pkg, files, value = TRUE)
  sapply(files, install_mran_single, ..., lib=lib, destdir=destdir, dependencies=dependencies)
}

install_mran_single <- function(pkg, date=NULL, lib = NULL, destdir = NULL, ..., dependencies = TRUE){
  path <- file.path(lib, pkg)
  pkgname <- strsplit(strsplit(pkg, "/")[[1]][ length(strsplit(pkg, "/")[[1]]) ], "_")[[1]][[1]]
  tmpdir <- tempdir()
  untar(path, exdir = tmpdir)
  path2 <- file.path(tmpdir, pkgname)
  info <- pkg_deps(path2)
  download_deps(pkg = path2, info=info, lib=lib, dependencies = dependencies)
  depscheck <- Map(needs_install, info$name, info$compare, info$version, lib)
  depsinstall <- info$name[as.logical(depscheck)]
  just_deps <- sapply(depsinstall, function(x) grep(x, list.files(lib, pattern = ".tar.gz"), value = TRUE))
  just_deps_paths <- file.path(lib, just_deps)
  # install dependencies
  install.packages(just_deps_paths, ..., lib=lib, dependencies=TRUE, repos=NULL, type = "source")
  # install target package
  install.packages(file.path(lib, grep(pkgname, list.files(lib, pattern = ".tar.gz"), value = TRUE)), lib=lib, dependencies=FALSE, repos=NULL, type = "source", ...)
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

download_deps <- function (pkg = NULL, info, lib, dependencies = NA) 
{
  pkg <- as.package(pkg)
#   info <- pkg_deps(pkg, dependencies)
  needed <- Map(needs_install, info$name, info$compare, info$version, lib=lib)
  deps <- info$name[as.logical(needed)]
  if (length(deps) == 0) return(invisible())
  message("Downloading dependencies for ", pkg$package, ":\n", paste(deps, collapse = ", "))
  pkgs_mran(date=date, pkgs=deps, outdir=lib)
  invisible(deps)
}

needs_install <- function(pkg, compare, version, lib) {
  if (length(find.package(pkg, lib=lib, quiet = TRUE)) == 0) return(TRUE)
  if (is.na(compare)) return(FALSE)
  compare <- match.fun(compare)
  !compare(packageVersion(pkg, lib=lib), version)
}

# install_mran <- function (pkg, snapshot=NULL, ..., dependencies = TRUE) 
# {
#   invisible(vapply(repo, install_github_single, FUN.VALUE = logical(1), ..., dependencies = TRUE))
# }
# 
# install_mran_single <- function (repo, username = getOption("github.user"), ref = "master", 
#           pull = NULL, subdir = NULL, branch = NULL, auth_user = NULL, 
#           password = NULL, auth_token = NULL, ...) 
# {
#   install_url(conn$url, subdir = conn$subdir, config = conn$auth, ...)
# }
# 
# install_url <- function (url, name = NULL, subdir = NULL, config = list(), before_install = NULL, ...) 
# {
#   if (is.null(name)) {
#     name <- rep(list(NULL), length(url))
#   }
#   invisible(mapply(install_url_single, url, name, MoreArgs = list(subdir = subdir, 
#     config = config, before_install = before_install, ...)))
# }
# 
# install_url_single <- function (url, name = NULL, subdir = NULL, config = list(), before_install = NULL, ...) 
# {
#   if (is.null(name)) {
#     name <- basename(url)
#   }
#   message("Downloading ", name, " from ", url)
#   bundle <- file.path(tempdir(), name)
#   request <- GET(url, config)
#   stop_for_status(request)
#   writeBin(content(request), bundle)
#   on.exit(unlink(bundle), add = TRUE)
#   install_local_single(bundle, subdir = subdir, before_install = before_install, ...)
# }
# 
# install_local_single <- function (path, subdir = NULL, before_install = NULL, ..., quiet = FALSE) 
# {
#   stopifnot(file.exists(path))
#   if (!quiet) {
#     message("Installing package from ", path)
#   }
#   if (!file.info(path)$isdir) {
#     bundle <- path
#     outdir <- tempfile(pattern = "devtools")
#     dir.create(outdir)
#     on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
#     path <- decompress(path, outdir)
#   }
#   else {
#     bundle <- NULL
#   }
#   pkg_path <- if (is.null(subdir)) 
#     path
#   else file.path(path, subdir)
#   if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
#     stop("Does not appear to be an R package (no DESCRIPTION)", 
#          call. = FALSE)
#   }
#   config_path <- file.path(pkg_path, "configure")
#   if (file.exists(config_path)) {
#     Sys.chmod(config_path, "777")
#   }
#   if (!is.null(bundle) && !is.null(before_install)) 
#     before_install(bundle, pkg_path)
#   install(pkg_path, quiet = quiet, ...)
# }