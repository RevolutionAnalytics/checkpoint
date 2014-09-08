#' Function to write manifest file
#'
#' @export
#' @inheritParams rrt_install

#' @param packs Packages used in the repo
#' @param repoid Respository ID
#' @param reponame repo name
#' @param author Authors, comma separated
#' @param license License, e.g., MIT
#' @param description Character description of repo
#' @param remote Remote git/svn/mercurial repo
#'
#' @keywords internal
#' @return Writes a RRT manifest file ("rrt_manifest.yml") to disk
writeManifest <- function(repo, libPath=rrtPath(repo, "lib"), packs, repoid, reponame="", author="",
                          license="", description="", remote="", snapshot=NULL, verbose=FALSE)
{
  # look first for user manifest file in root directory - overrides user input for fields:
  ## reponame, author, license, description, and remote
  giveZeroChar <- function(x) if(is.null(x)) "" else x
  usermanfile <- normalizePath(file.path(repo, "manifest.yml"))
  if(file.exists(usermanfile)){
    tt <- yaml.load_file(usermanfile)
    if(!is.null(tt)){
      reponame <- giveZeroChar(tt$RepositoryName)
      author <- giveZeroChar(tt$Authors)
      license <- giveZeroChar(tt$License)
      description <- giveZeroChar(tt$Description)
      remote <- giveZeroChar(tt$Remote)
    }
  }
  
  reponame <- sprintf("RepositoryName: %s", reponame)
  author   <- sprintf("Authors: %s", author)
  license  <- sprintf("License: %s", license)
  description <- sprintf("Description: %s", description)
  remote      <- sprintf("Remote: %s", remote)

  infofile      <- rrtPath(repo, "manifest")
  installedwith <- "InstalledWith: RRT"
  installedfrom <- "InstalledFrom: source"
  rrtver        <- sprintf("RRT_version: %s", packageVersion("RRT"))

  snaps <- if(is.null(snapshot)) getOption("RRT_snapshotID", "") else snapshot
  rrtsnapshot <- sprintf("RRT_snapshotID: %s", snaps)

  rver <- sprintf("R_version: %s", paste0(as.character(R.version[c('major','minor')]), collapse="."))
  pkgsloc <- sprintf("PkgsInstalledAt: %s", libPath)
  
  
  mssg(verbose, "... checking for package system requirements")
  sysreq <- sprintf("SystemRequirements:\n%s", getSysreq(packs, srcPath=rrtPath(repo, "src")))

  repositoryid <- sprintf("RepoID: %s", repoid)

  mssg(verbose, "... writeManifest: checking for date created")

  datecheck <- check4date_created(x=infofile)
  date_created <- if(is.null(datecheck)) {
    sprintf("DateCreated: %s", format(Sys.time(), "%Y-%m-%d")) 
  } else { 
    datecheck
  }
  date_updated <- sprintf("DateUpdated: %s", format(Sys.time(), "%Y-%m-%d"))

  mssg(verbose, "... writeManifest: checking for github")
  github <- check4github(infofile)

  info <- c(reponame, author, license, description, remote, 
            installedwith, installedfrom, rrtsnapshot, rrtver, rver, 
            date_created, date_updated, path.expand(pkgsloc), 
            repositoryid, sysreq, github)
  mssg(verbose, "... writeManifest: writing manifest files")
  cat(info, file = infofile, sep = "\n")
}

check4date_created <- function(x){
  if(file.exists(x)){
    info <- readLines(x)
    dcline <- grep("DateCreated", info, value = TRUE)
    if(length(dcline) > 0) dcline else NULL
  } else { NULL }
}

check4github <- function(x){
  if(file.exists(x)){
    info <- suppressWarnings(yaml.load_file(x))
    gline <- info['Github'][1]
    if(!is.null(gline[[1]])){
      sprintf("Github:\n- %s", gline[[1]])
    } else { NULL }
  } else { NULL }
}


#' Function to get system requirements, if any, from installed packages
#'
#' @param x (character) Name of package. One to many in a vector or list
#' @param lib libPath to look in
#' @keywords internal
#' @examples \dontrun{
#' getsysreq('RCurl')
#' getsysreq(c('RCurl','doMC','ggplot2','XML','rgdal'))
#' }

getSysreq <- function(pkgs, srcPath){
  tmp <- lapply(pkgs, function(pkg) getSysreqFromDescriptionFile(pkg, srcPath = srcPath))
  names(tmp) <- pkgs
  tmp <- rrt_compact(tmp)
  if(!length(tmp) == 0){
    tt <- list()
    for(i in seq_along(tmp)){
      tt[[i]] <- paste(sprintf(" - %s:", names(tmp[i])), gsub("\n", " ", tmp[[i]]))
    }
    vv <- paste(tt, collapse = "\n")
    if(length(vv) == 0) "" else vv
  } else { if(length(tmp) == 0) "" else tmp }
}

getSysreqFromDescriptionFile <- function(pkg, srcPath){
  tmpdir <- tempdir()
  tarfiles <- list.files(srcPath, pattern = ".tar.gz", full.names = TRUE)
  use <- grep(rr, tarfiles, value = TRUE)
  if(length(use) == 0){ return(NULL) } else {
    if(length(use) > 1){
      pkgs <- gsub("_.+", "", sapply(use, 
                                     function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])], 
                                     USE.NAMES = FALSE))
      use <- use[pkgs %in% pkg]
    }  
    untar(use, exdir = tmpdir)
    out <- as.package(file.path(tmpdir, pkg))
    sysreq <- out['systemrequirements'][[1]]
    if(is.null(sysreq)) NULL else sysreq
  }
}