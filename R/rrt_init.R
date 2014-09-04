#' Initiate a RRT repository.
#'
#' This function initiates a repository. You can run this function to start a new repository, without any work done yet, creating a new folder and RRT files, or you can initiate a RRT repository inside an existing project/folder you already have. If the latter, we don't alter your files at all, but simply write a few files needed for RRT to work properly. By detault repo initialization is done interactively, so that you can choose your settings or accept reasonable defaults.
#'
#' @export
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory.
#' @param mran (logical) If TRUE (default), packages are installed from the MRAN server. See \url{http://mran.revolutionanalytics.com} for more information.
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
  
  # Look for packages in the project
  mssg(verbose, "Looking for packages used in your repository...")
  pkgs <- scanRepoPackages(repo)
  pkgs <- repoDependencies(pkgs, simplify = TRUE, base=FALSE, suggests=suggests)
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
  snapshot <- setSnapshotInOptions(repo, snapdate=snapdate, autosnap=autosnap)

  # Write new .Rprofile file
  if(is.null(rprofile)){
    rprofilepath <- file.path(repo, ".Rprofile")
    mirror <- 'options(repos = c(CRAN = "http://cran.revolutionanalytics.com/"))'
    libpaths <- sprintf(".libPaths(c('%s'))", paste0(c(libPath, .libPaths()), collapse = "','"))
    
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
  rrt_install(repo, snapshot=snapshot, libPath=libPath, suggests=suggests, verbose=verbose, quiet=quiet)

  # Write blank user manifest file, or not if already present
  writeUserManifest(repo = repo, verbose = verbose)

  # Write to internal manifest file
  mssg(verbose, "Writing repository locked manifest file...")
  writeManifest(repo = repo, libPath = libPath, packs = pkgs, 
                repoid=repoid, reponame=reponame, author=author, 
                license=licence, description=description, remote=remote, 
                snapshot = getOption('RRT_snapshotID'), verbose=verbose)

  # write package versions to manifest file
  write_pkg_versions(libPath, repo)
  
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


write_pkg_versions <- function(libPath, repo){
  instPkgs <- list.files(normalizePath(libPath), full.names = TRUE)
  if(!length(instPkgs) == 0){
    names(instPkgs) <- list.files(normalizePath(libPath))
    instPkgs <- setdiff(instPkgs, "src")
    instPkgs_ver <- lapply(instPkgs, function(x) as.package(x)$version)
    out <- sprintf("Packages:\n%s", catPkgVersion(instPkgs_ver) )
    cat(out, 
        file = rrtPath(repo, "manifest"), 
        sep = "\n", append = TRUE)
  }
}

catPkgVersion <- function(x){
  tt <- list()
  for(i in seq_along(x)){
    tt[[i]] <- paste0(sprintf(" - %s~", names(x[i])), gsub("\n", " ", x[[i]]))
  }
  vv <- paste(tt, collapse = "\n")
  if(length(vv) == 0) "" else vv
}

writeUserManifest <- function(repo, verbose){
  usermanfile <- suppressWarnings(normalizePath(file.path(repo, "manifest.yml")))
  if(file.exists(usermanfile)){
    mssg(verbose, "User manifest file already exists")
  } else {
    mssg(verbose, "Writing blank user manifest file...")
    fields <- c('RepositoryName:', 'Authors:', 'License:', 'Description:', 'Remote:')
    cat(fields, file = usermanfile, sep = "\n")
  }
}

getSnapshotFromManifest <- function(repo){
  manfile <- rrtPath(repo, "manifest")
  if(file.exists(manfile)){
    manfile_contents <- yaml.load_file(manfile)
    manfile_contents$RRT_snapshotID
  }
}

setSnapshotInOptions <- function(repo, snapdate, autosnap=TRUE){
  if(is.null(snapdate)){
    tt <- getSnapshotFromManifest(repo=repo)
    if(!is.null(tt)){ snapshotId <- tt } else {
      if(autosnap){
        availsnaps <- mranSnapshots(message=FALSE)
        snapshotId <- availsnaps[length(availsnaps)]
      } else { stop("You must provide a date or set autosnap=TRUE") }
    }
  } else {
    # as of 2014-07-11, we're moving to one snapshot per day, so forcing to last
    # snapshot of any day if there are more than 1
    snapshotId <- getSnapshotId(snapdate, forceLast = TRUE)
  }
  options(RRT_snapshotID = snapshotId)
  snapshotId
}
