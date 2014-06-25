#' Function to write manifest file
#'
#' # FIXME: check for existence of manifest file first, and combine with old info, if any
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
                          license="", description="", remote="")
{
  reponame <- sprintf("RepositoryName: %s", reponame)
  author <- sprintf("Authors: %s", author)
  license <- sprintf("License: %s", license)
  description <- sprintf("Description: %s", description)
  remote <- sprintf("Remote: %s", remote)

  infofile <- file.path(repository, "rrt", "rrt_manifest.yml")
  installedwith <- "InstalledWith: RRT"
  installedfrom <- "InstalledFrom: source"
  rrtver <- sprintf("RRT_version: %s", packageVersion("RRT"))

  rrtsnapshot <- sprintf("RRT_snapshotID: %s", getOption("RRT_snapshotID", ""))

  rver <- sprintf("R_version: %s", paste0(as.character(R.version[c('major','minor')]), collapse="."))
  pkgsloc <- sprintf("PkgsInstalledAt: %s", librar)
#   sysreq <- sprintf("SystemRequirements: %s", paste0(rrt_compact(getsysreq(packs, lib=librar)), collapse = "\n") )
  sysreq <- sprintf("SystemRequirements:\n%s", getsysreq(packs, lib=librar) )
  pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ", "))
  repositoryid <- sprintf("RepoID: %s", repoid)
  datecheck <- check4date_created(x=infofile)
  date_created <- if(is.null(datecheck)) sprintf("DateCreated: %s", format(Sys.time(), "%Y-%m-%d")) else datecheck
  date_updated <- sprintf("DateUpdated: %s", format(Sys.time(), "%Y-%m-%d"))

  github <- check4github(infofile)

  info <- c(reponame, author, license, description, remote, installedwith, installedfrom, rrtsnapshot, rrtver,
            rver, date_created, date_updated, path.expand(pkgsloc), repositoryid, pkgs_deps, sysreq, github)
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
    info <- yaml.load_file(x)
    gline <- info['Github'][1]
    if(!is.null(gline[[1]])){
      sprintf("Github:\n- %s", gline[[1]])
    } else { NULL }
  } else { NULL }
}

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
    paste(tt, collapse = "\n")
  } else { tmp }
}

pkgdesc <- function(rr, lib){
  tmpdir <- tempdir()
  tarfiles <- list.files(file.path(lib, "src/contrib"), pattern = ".tar.gz", full.names = TRUE)
  use <- grep(rr, tarfiles, value = TRUE)
  if(length(use) > 1){
    pkgs <- gsub("_.+", "", sapply(use, function(x) strsplit(x, "/")[[1]][length(strsplit(x, "/")[[1]])], USE.NAMES = FALSE))
    use <- use[pkgs %in% rr]
  }
  untar(use, exdir = tmpdir)
  out <- as.package(file.path(tmpdir, rr))
  sysreq <- out['systemrequirements'][[1]]
#   lines <- yaml.load_file(file.path(tmpdir, rr, "DESCRIPTION"))
#   sysreq <- lines['SystemRequirements'][[1]]
  if(is.null(sysreq)) NULL else sysreq
}
