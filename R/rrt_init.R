#' Initiate a RRT repository.
#'
#' This function initiates a repository.
#'
#' @import digest miniCRAN
#' @export
#' 
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See 
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo.
#' @param interactive (logical) If TRUE (default), function asks you for input for each item,
#' otherwise, defaults are used.
#'
#' @return Files written to the user's machine, with informative messages on progress
#' 
#' @examples \dontrun{
#' rrt_init(repo="~/testrepo")
#' rrt_refresh(repo="~/testrepo")
#' rrt_refresh(repo="~/testrepo", mran=TRUE)
#' rrt_install(repo="~/testrepo")
#'
#' # Optionally, do an interactive repo intitialization
#' rrt_init(repo="~/mynewcoolrepo", interactive=TRUE)
#' }

rrt_init <- function(repo=getwd(), mran=FALSE, snapdate=NULL, verbose=TRUE, rprofile=NULL, interactive=FALSE)
{
  if(interactive){
    message("\nRepository name (default: random name generated):")
    randomname <- paste0(sample(letters, 10), collapse = "")
    reponame <- rrt_readline(randomname)

    message("\nRepository path (default: home directory + repository name):")
    defaultpath <- file.path(Sys.getenv("HOME"), reponame)
    repo <- rrt_readline(defaultpath)
  } else {
    if(is.null(repo)){
      stop("You need to specify a repository path and name")
    } else {
      reponame <- strsplit("~/testrepo", "/")[[1]][length(strsplit("~/testrepo", "/")[[1]])]
    }
  }

  if(interactive){
    message("\nRepository author(s) (default: left blank):")
    author <- rrt_readline()
    message("\nRepo license (default: MIT):")
    license <- rrt_readline("MIT")
    message("\nRepo description (default: left blank):")
    description <- rrt_readline()
    message("\nRepo remote git or svn repo (default: left blank):")
    remote <- rrt_readline()
  } else {
    author <- description <- remote <- ""
    license <- "MIT"
  }

  # create repo id using digest
  repoid <- digest(repo)

  # create repo
  mssg(verbose, "Checking to see if repository exists already...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    mssg(verbose, sprintf("Creating repository %s", repo))
    dir.create(repo)
  }

  # check for rrt directory in the repo, and create if doesn't exist already
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)
  ## ignore git dir
  present <- present[!grepl(".git", present)]
  if(!all(grepl("rrt", present))){
    mssg(verbose, sprintf("Creating rrt directory %s", lib))
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  }

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)

  # get packages in a private location for this project
  getPkgs(x = pkgs, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repo, lib, pkgs, repoid, reponame, author, license, description, remote)

  # Write repo log file
  rrt_repos_write(repo, repoid)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    mirror <- 'options(repos=structure(c(CRAN="http://cran.revolutionanalytics.com/")))'
    libpaths <- sprintf('.libPaths("%s")', lib)
    msg <- sprintf("cat('    Starting repo from RRT repository: %s \n    Packages will be installed in and loaded from this repository\n    To go back to a non-RRT environment, start R outside an RRT repository\n    Report any bugs/feature requests at https://github.com/RevolutionAnalytics/RRT\n\n')", repoid)
    cat(c(mirror, libpaths, msg), file=rprofilepath, sep="\n")
  } else {
    NULL # fixme: add ability to write options to the rprofile file
  }

  message("\n>>> RRT initialization completed.")
}

rrt_readline <- function(default=""){
  tmp <- readline()
  if(nchar(tmp) == 0) default else tmp
}

#' Refresh package - look for any new packages used and install those in rrt library
#'
#' @export
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See 
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
rrt_refresh <- function(repo=getwd(), mran=FALSE, snapdate=NULL, verbose=TRUE)
{
  repoid <- digest(repo)

  # check to make sure repo exists
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){ # only create if file doesn't exist already
    stop(sprintf("Repository %s doesn't exist", repo))
  }

  # check for rrt directory in the repo, and stop if it doesn't exist
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)[-1]
  ## ignore git dir
  present <- present[!grepl(".git", present)]
  if(!all(grepl("rrt", present))){
    stop("rrt directory doesn't exist")
  }

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)

  # get packages in a private location for this project
  mssg(verbose, "Getting new packages...")
  getPkgs(x = pkgs, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repo, lib, pkgs, repoid)

  message("\n>>> RRT refresh completed.")
}

#' Function to install pkgs
#'
#' @import miniCRAN
#' @export
#' @param x (character) A vector of package names. If NULL, none installed, and message prints
#' @param lib (character) Library location, a directory
#' @param recursive (logical) Recursively install packages?
#' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
#' @param install (logical) Install packages or just download packages. Not used yet...
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See 
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @examples \dontrun{
#' getPkgs()
#' }
getPkgs <- function(x, lib, recursive=FALSE, verbose=TRUE, install=TRUE, mran=FALSE, snapdate=NULL){
  # check for existence of pkg, subset only those that need to be installed
  if(is.null(x)){ NULL } else {

    pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
    if(!file.exists(pkgslist)) { pkgs2install <- x } else {
      installedpkgs <- gsub("Package:\\s", "", grep("Package:", readLines(pkgslist), value=TRUE))
      pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
    }

    # Make local repo of packages
    if(!is.null(pkgs2install) || length(pkgs2install) == 0){
      if(!mran){
        # FIXME, needs some fixes on miniCRAN to install source if binaries not avail.-This may be fixed now
        makeRepo(pkgs = pkgs2install, path = lib, download = TRUE)
      } else {
        if(is.null(snapdate)) snapdate <- Sys.Date()
        snapdate <- "2014-06-19"
#         snapdate <- as.POSIXct(Sys.Date())
#         format(snapdate, tz="Europe/London", usetz=TRUE)
        pkgloc <- file.path(lib, "src/contrib")
        setwd(lib)
        dir.create("src/contrib", recursive = TRUE)
        pkgs_mran(date=snapdate, pkgs=pkgs2install, outdir=pkgloc)
      }
    } else {
      return(mssg(verbose, "No packages found - none installed"))
    }
  }
}

#' Install packages
#'
#' Installs packages to your local repository. Performs some checks to make sure your repo is a
#' RRT repository, and the rrt dir exists with your repository, then installs packages not already
#' installed.
#'
#' @export
#' @param repo A repository path
#' @param verbose Print messages
#' rrt_install(repo="~/testrepo")
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
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)[-1]
  ## ignore git dir
  present <- present[!grepl(".git", present)]
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

#' Function to get system requireqments, if any, from installed packages
#'
#' @export
#' @param x (character) Name of package. One to many in a vector or list
#' @keywords internal
#' @examples \dontrun{
#' getsysreq('RCurl')
#' getsysreq(c('RCurl','doMC','ggplot2','XML','rgdal'))
#' }

getsysreq <- function(x)
{
  tmp <-
    lapply(x, function(y){
      res <- packageDescription(y, encoding = NA)
      if(is(res, "packageDescription")) res$SystemRequirements
    })
  names(tmp) <- x
  tmp
}

#' Function to write manifest file
#'
#' # FIXME: check for existence of manifest file first, and combine with old info, if any
#'
#' @export
#' @keywords internal
#' @return Writes a RRT manifest file to disc
writeManifest <- function(repository, librar, packs, repoid, reponame="", author="", license="", description="", remote=""){
  reponame <- sprintf("RepositoryName: %s", reponame)
  author <- sprintf("Authors: %s", author)
  license <- sprintf("License: %s", license)
  description <- sprintf("Description: %s", description)
  remote <- sprintf("Remote: %s", remote)

  infofile <- file.path(repository, "rrt", "rrt_manifest.txt")
  installedwith <- "InstalledWith: RRT"
  installedfrom <- "InstalledFrom: source"
  rrtver <- sprintf("RRT_version: %s", packageVersion("RRT"))
  rver <- sprintf("R_version: %s", paste0(as.character(R.version[c('major','minor')]), collapse="."))
  pkgsloc <- sprintf("PkgsInstalledAt: %s", librar)
  sysreq <- sprintf("SystemRequirements: %s", paste0(rtt_compact(getsysreq(packs)), collapse = "\n") )
  pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ", "))
  repositoryid <- sprintf("RepoID: %s", repoid)
  date <- sprintf("DateCreated: %s", format(Sys.time(), "%Y-%m-%d"))
  info <- c(reponame, author, license, description, remote, installedwith, installedfrom, rrtver,
            rver, date, path.expand(pkgsloc), repositoryid, pkgs_deps, sysreq)
  cat(info, file = infofile, sep = "\n")
}
