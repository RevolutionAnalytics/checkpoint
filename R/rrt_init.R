#' Initiate a RRT repository.
#'
#' This function initiates a repository. You can run this function to start a new repository, without any work done yet, creating a new folder and RRT files, or you can initiate a RRT repository inside an existing project/folder you already have. If the latter, we don't alter your files at all, but simply write a few files needed for RRT to work properly. By detault repo initialization is done interactively, so that you can choose your settings or accept reasonable defaults.
#'
#' @import digest
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See
#' \url{http://mran.revolutionanalytics.com} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' @param autosnap (logical) Get most recent snapshot. Default: FALSE
#' @param verbose (logical) Whether to print messages or not (Default: TRUE).
#' @param rprofile (list) pass in a list of options to include in the .Rprofile file for the repo.
#' @param interactive (logical) If TRUE, function asks you for input for each item, otherwise, defaults are used. Default: FALSE.
#' @param suggests (logical) Download and install packages in the Suggests line for packages used in your RRT repository, or not. Default: FALSE.
#' @param quiet Passed to \code{\link[utils]{install.packages}}
#'
#' @family rrt
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

rrt_init <- function(repo=getwd(), mran=TRUE, snapdate=NULL, autosnap=FALSE, verbose=TRUE,
                     rprofile=NULL, interactive=FALSE, suggests=FALSE, quiet=FALSE)
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
      reponame <- strsplit(repo, "/")[[1]][length(strsplit(repo, "/")[[1]])]
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
  repoid <- digest(suppressWarnings(normalizePath(repo)))

  # create repo
  makeRRTrepo(repo, verbose)

  # check for rrt directory in the repo, and create if doesn't exist already
  # and create library directory
  lib <- rrt_libpath(repo)
  check4rrt(repo, lib, verbose)

  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)
  mssg(verbose, sprintf("... %s", paste(pkgs, collapse=", ")))
  ## remove packages not found on MRAN
  if(!is.null(pkgs)){
    notfound <- pkgs[grepl("not found", pkgs)]
    if(!length(notfound) == 0){
      cat(notfound, sep = "\n")
    }
    pkgs <- pkgs[!grepl("not found", pkgs)]
  }

  # Look for packages installed by user but no source available
  # if some installed give back vector of package names
  addtnpkgs <- checkUserInstall(lib)
  pkgs <- c(addtnpkgs, pkgs)

  # get packages in a private location for this project
#   mssg(verbose, "Getting new packages...")
  set_snapshot_date(repo, snapdate, autosnap)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    mirror <- 'options(repos = c(CRAN = "http://cran.revolutionanalytics.com/"))'
    libpaths <- sprintf(".libPaths(c('%s'))", paste0(c(lib, .libPaths()), collapse = "','"))
    
    msg <- paste0(
      "cat('",
      "    Starting repo from RRT repository: %s \n",
      "    Packages installed in and loaded from this repository\n",
      "    To go back to a non-RRT environment, start R outside an RRT repository\n\n",
      "')")
    
    msg <- sprintf(msg, repoid)
    
    cat(c(mirror, libpaths, msg), file=rprofilepath, sep="\n")
  } else {
    NULL # fixme: add ability to write options to the rprofile file
  }

  # install packages
  rrt_install(repo, repoid=repoid, lib=lib, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write blank user manifest file, or not if already present
  writeUserManifest(repository = repo, verbose = verbose)

  # Write to internal manifest file
  mssg(verbose, "Writing repository locked manifest file...")
  writeManifest(repository = repo, librar = lib, packs = pkgs, 
                repoid=repoid, reponame=reponame, author=author, 
                license=licence, description=description, remote=remote, 
                snapshot = getOption('RRT_snapshotID'), verbose=verbose)

  # write package versions to manifest file
  write_pkg_versions(lib, repo)
  
  # Write repo log file
  mssg(verbose, "Writing repository log file...")
  rrt_repos_write(repo, repoid)

  # regenerate RRT dashboard
  rrt_browse(browse = FALSE)

mssg(verbose, "\n>>> RRT initialization completed.")
}

rrt_readline <- function(default=""){
  tmp <- readline()
  if(nchar(tmp) == 0) default else tmp
}


write_pkg_versions <- function(lib, repo){
  instpks <- list.files(normalizePath(lib), full.names = TRUE)
  if(!length(instpks) == 0){
    names(instpks) <- list.files(normalizePath(lib))
    instpks <- instpks[!names(instpks) %in% "src"]
    instpks_ver <- lapply(instpks, function(x) as.package(x)$version)
    towrite <- sprintf("Packages:\n%s", cat_pack_vers(instpks_ver) )
    cat(towrite, 
        file = normalizePath(file.path(repo, "rrt/rrt_manifest.yml")), 
        sep = "\n", append = TRUE)
  }
}

cat_pack_vers <- function(x)
{
  tt <- list()
  for(i in seq_along(x)){
    tt[[i]] <- paste0(sprintf(" - %s~", names(x[i])), gsub("\n", " ", x[[i]]))
  }
  vv <- paste(tt, collapse = "\n")
  if(length(vv) == 0) "" else vv
}

writeUserManifest <- function(repository, verbose){
  usermanfile <- suppressWarnings(normalizePath(file.path(repository, "manifest.yml")))
  if(!file.exists(usermanfile)){
    mssg(verbose, "Writing blank user manifest file...")
    fields <- c('RepositoryName:', 'Authors:', 'License:', 'Description:', 'Remote:')
    cat(fields, file = usermanfile, sep = "\n")
  } else { mssg(verbose, "User manifest file already exists") }
}

get_snapshot_date <- function(repository){
  manfile <- suppressWarnings(normalizePath(file.path(repository, "rrt/rrt_manifest.yml")))
  if(file.exists(manfile)){
    manfile_contents <- yaml.load_file(manfile)
    manfile_contents$RRT_snapshotID
  }
}

set_snapshot_date <- function(repo, snapdate, autosnap){
  if(is.null(snapdate)){
    tt <- get_snapshot_date(repository=repo)
    if(!is.null(tt)){ snapshotid <- tt } else {
      if(autosnap){
        availsnaps <- suppressMessages(mran_snaps())
        snapshotid <- availsnaps[length(availsnaps)]
      } else { stop("You must provide a date or set autosnap=TRUE") }
    }
  } else {
    # as of 2014-07-11, we're moving to one snapshot per day, so forcing to last
    # snapshot of any day if there are more than 1
    snapshotid <- getsnapshotid(snapdate, forcelast = TRUE)
  }
  options(RRT_snapshotID = snapshotid)
}
