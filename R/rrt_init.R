#' Initiate a RRT repository.
#'
#' This function initiates a repository. You can run this function to start a new repository,
#' without any work done yet, creating a new folder and RRT files, or you can initiate a RRT
#' repository inside an existing project/folder you already have. If the latter, we don't alter
#' your files at all, but simply write a few files needed for RRT to work properly. By detault
#' repo initialization is done interactively, so that you can choose your settings or accept
#' reasonable defaults.
#'
#' @import digest miniCRAN
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo.
#' @param interactive (logical) If TRUE, function asks you for input for each item,
#' otherwise, defaults are used. Default: FALSE.
#'
#' @seealso \link{rrt_refresh}, \link{rrt_install}
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

rrt_init <- function(repo=getwd(), mran=TRUE, snapdate=NULL, verbose=TRUE, rprofile=NULL, interactive=FALSE)
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
  # and create library directory
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, base::getRversion())
  if(!file.exists(file.path(repo, "rrt"))){
    mssg(verbose, sprintf("Creating rrt directory %s", lib))
    dir.create(lib, showWarnings = FALSE, recursive = TRUE)
  } else { mssg(verbose, "rrt directory already exists") }

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=TRUE)

  # get packages in a private location for this project
  getPkgs(x = pkgs, lib = lib, verbose = verbose, mran = mran, snapdate = snapdate)

  # Write to internal manifest file
  mssg(verbose, "Writing repository manifest...")
  writeManifest(repository = repo, librar = lib, packs = pkgs, repoid, reponame, author, license, description, remote)

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

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

  message("\n>>> RRT initialization completed.")
}

rrt_readline <- function(default=""){
  tmp <- readline()
  if(nchar(tmp) == 0) default else tmp
}
