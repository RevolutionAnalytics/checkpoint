#' Function to write manifest file
#'
#' @export
#' @param repository Repository root path
#' @param librar Library to install packages in
#' @param packs Packages used in the repository
#' @param repoid Respository ID
#' @param reponame Repository name
#' @param author Authors, comma separated
#' @param license License, e.g., MIT
#' @param description Character description of repository
#' @param remote Remote git/svn/mercurial repository
#'
#' @keywords internal
#' @return Writes a RRT manifest file ("rrt_manifest.yml") to disk
writeManifest <- function(repository, librar, packs, repoid, reponame="", author="",
                          license="", description="", remote="", snapshot=NULL, verbose=FALSE)
{
  # look first for user manifest file in root directory - overrides user input for fields:
  ## reponame, author, license, description, and remote
  givezerochar <- function(x) if(is.null(x)) "" else x
  usermanfile <- normalizePath(file.path(repository, "manifest.yml"))
  if(file.exists(usermanfile)){
    tt <- yaml.load_file(usermanfile)
    if(!is.null(tt)){
      reponame <- givezerochar(tt$RepositoryName)
      author <- givezerochar(tt$Authors)
      license <- givezerochar(tt$License)
      description <- givezerochar(tt$Description)
      remote <- givezerochar(tt$Remote)
    }
  }
  
  reponame <- sprintf("RepositoryName: %s", reponame)
  author <- sprintf("Authors: %s", author)
  license <- sprintf("License: %s", license)
  description <- sprintf("Description: %s", description)
  remote <- sprintf("Remote: %s", remote)

  infofile <- file.path(repository, "rrt", "rrt_manifest.yml")
  installedwith <- "InstalledWith: RRT"
  installedfrom <- "InstalledFrom: source"
  rrtver <- sprintf("RRT_version: %s", packageVersion("RRT"))

  snaps <- if(is.null(snapshot)) getOption("RRT_snapshotID", "") else snapshot
  rrtsnapshot <- sprintf("RRT_snapshotID: %s", snaps)

  rver <- sprintf("R_version: %s", paste0(as.character(R.version[c('major','minor')]), collapse="."))
  pkgsloc <- sprintf("PkgsInstalledAt: %s", librar)
  
  
  mssg(verbose, "... checking for package system requirements")
#   sysreq <- sprintf("SystemRequirements: %s", paste0(rrt_compact(getsysreq(packs, lib=librar)), collapse = "\n") )
  sysreq <- sprintf("SystemRequirements:\n%s", getsysreq(packs, lib=librar) )
#   pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ", "))

#   pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse=", "))
  repositoryid <- sprintf("RepoID: %s", repoid)

  mssg(verbose, "... writeManifest: checking for date created")

  datecheck <- check4date_created(x=infofile)
  date_created <- if(is.null(datecheck)) sprintf("DateCreated: %s", format(Sys.time(), "%Y-%m-%d")) else datecheck
  date_updated <- sprintf("DateUpdated: %s", format(Sys.time(), "%Y-%m-%d"))

  mssg(verbose, "... writeManifest: checking for github")
  github <- check4github(infofile)

  info <- c(reponame, author, license, description, remote, installedwith, installedfrom, rrtsnapshot, rrtver,
            rver, date_created, date_updated, path.expand(pkgsloc), repositoryid, sysreq, github)
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

# check4pkgs <- function(x, repo){
#   manfile <- file.path(repo, "rrt/rrt_manifest.yml")
#   x <- if(is.null(x)) NULL else x
#   if(file.exists(manfile)){
#     info <- suppressWarnings(yaml.load_file(manfile))
#     pline <- info['Packages'][1]
#     if(!is.null(pline[[1]])){
#       bb <- gsub("\\s", "", strsplit(pline[[1]], ",")[[1]])
#       paste0(unique(c(x,bb)), collapse = ", ")
#     } else { paste0(x, collapse = ", ") }
#   } else { NULL }
# }

#' Function to get system requireqments, if any, from installed packages
#'
#' @param x (character) Name of package. One to many in a vector or list
#' @param lib Library to look in
#' @keywords internal
#' @examples \dontrun{
#' getsysreq('RCurl')
#' getsysreq(c('RCurl','doMC','ggplot2','XML','rgdal'))
#' }

getsysreq <- function(x, lib)
{
  tmp <- lapply(x, function(y) pkgdesc(y, lib = lib))
  names(tmp) <- x
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

pkgdesc <- function(rr, lib){
  tmpdir <- tempdir()
  tarfiles <- list.files(file.path(lib, "src/contrib"), pattern = ".tar.gz", full.names = TRUE)
  use <- grep(rr, tarfiles, value = TRUE)
  if(length(use) == 0){ return(NULL) } else {
    if(length(use) > 1){
      pkgs <- gsub("_.+", "", sapply(use, function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])], USE.NAMES = FALSE))
      use <- use[pkgs %in% rr]
    }  
    untar(use, exdir = tmpdir)
    out <- as.package(file.path(tmpdir, rr))
    sysreq <- out['systemrequirements'][[1]]
    if(is.null(sysreq)) NULL else sysreq
  }
}