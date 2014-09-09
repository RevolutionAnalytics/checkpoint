#' Initiate a RRT repository.
#'
#' This function initiates a repository. You can run this function to start a new repository, without any work done yet, creating a new folder and RRT files, or you can initiate a RRT repository inside an existing project/folder you already have. If the latter, we don't alter your files at all, but simply write a few files needed for RRT to work properly. By detault repo initialization is done interactively, so that you can choose your settings or accept reasonable defaults.
#'
#' @export
#' 
#' @inheritParams checkpoint
#'
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

rrt_init <- function(repo=getwd(), snapshotdate=NULL, mran=TRUE, autosnap=FALSE, verbose=TRUE,
                     rprofile=NULL, interactive=FALSE, suggests=FALSE, quiet=FALSE)
{
  
  if(is.null(repo) && !interactive) stop("You need to specify a repository path and name")
  
  if(interactive){
    rrt_repo    <- rrtInteractive()
    
    repo        <- rrt_repo$repo
    reponame    <- rrt_repo$reponame
    author      <- rrt_repo$author
    description <- rrt_repo$description
    remote      <- rrt_repo$remote
    license     <- rrt_repo$license
  } else {
    reponame    <- basename(repo)
    author      <- ""
    description <- ""
    remote      <- ""
    license     <- "MIT"
  }
  
  # create repo id
  repoid <- repoDigest(repo)

  # create repo folders
  if(!file.exists(repo)) stop ("repo folder does not exist")
  createRepoFolders(repo)
  libPath <- rrtPath(repo, type = "lib")
  srcPath <- rrtPath(repo, type = "src")
  
  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- scanRepoPackages(repo)
#   pkgs <- repoDependencies(pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
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
  additionalPkgs <- checkUserInstall(libPath)
  pkgs <- c(additionalPkgs, pkgs)

  # get packages in a private location for this project
  # mssg(verbose, "Getting new packages...")
  snapshotid <- setSnapshotInOptions(repo, snapshotdate=snapshotdate, autosnap=autosnap)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    mirror <- 'options(repos = c(CRAN = "http://cran.revolutionanalytics.com/"))'
    libpaths <- sprintf(".libPaths(c('%s'))", 
                        paste0(c(libPath, .libPaths()), collapse = "','"))
    
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
  rrt_install(repo, snapshotid=snapshotid, libPath=libPath, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write blank user manifest file, or not if already present
  writeUserManifest(repo = repo, verbose = verbose)

  # Write to internal manifest file
  mssg(verbose, "Writing repository locked manifest file...")
  writeManifest(repo = repo, libPath = libPath, pkgs = pkgs, 
                repoid=repoid, reponame=reponame, author=author, 
                license=licence, description=description, remote=remote, 
                snapshot = getOption('RRT_snapshotID'), verbose=verbose)

  # write package versions to manifest file
  writePackagesToManifest(repo, libPath)
  
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


repoInstalledPackages <- function(repo, libPath=rrtPath(repo, "lib")){
  instPkgs <- list.files(libPath, full.names = TRUE, recursive = FALSE)
  names(instPkgs) <- list.files(libPath, recursive = FALSE)
  instPkgs[!names(instPkgs) == "src"]
}



setSnapshotInOptions <- function(repo, snapshotdate, autosnap=TRUE){
  if(is.null(snapshotdate)){
    tt <- getSnapshotFromManifest(repo=repo)
    if(!is.null(tt)){ snapshotId <- tt } else {
      if(autosnap){
        availsnaps <- mranSnapshots(verbose=FALSE)
        snapshotId <- availsnaps[length(availsnaps)]
      } else { stop("You must provide a date or set autosnap=TRUE") }
    }
  } else {
    # as of 2014-07-11, we're moving to one snapshot per day, so forcing to last
    # snapshot of any day if there are more than 1
    snapshotId <- getSnapshotId(snapshotdate, forceLast = TRUE)
  }
  options(RRT_snapshotID = snapshotId)
  snapshotId
}
