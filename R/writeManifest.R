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
  sysreq <- sprintf("SystemRequirements: %s", paste0(rtt_compact(getsysreq(packs)), collapse = "\n") )
  pkgs_deps <- sprintf("Packages: %s", paste0(packs, collapse = ", "))
  repositoryid <- sprintf("RepoID: %s", repoid)
  datecheck <- check4date_created(x=infofile)
  date_created <- if(is.null(datecheck)) sprintf("DateCreated: %s", format(Sys.time(), "%Y-%m-%d")) else datecheck
  date_updated <- sprintf("DateUpdated: %s", format(Sys.time(), "%Y-%m-%d"))
  
  info <- c(reponame, author, license, description, remote, installedwith, installedfrom, rrtsnapshot, rrtver,
            rver, date_created, date_updated, path.expand(pkgsloc), repositoryid, pkgs_deps, sysreq)
  cat(info, file = infofile, sep = "\n")
}

check4date_created <- function(x){
  if(file.exists(x)){
    info <- readLines(x)
    dcline <- grep("DateCreated", info, value = TRUE)
    if(length(dcline) > 0) dcline else NULL
  } else { NULL }
}

#' Function to get system requireqments, if any, from installed packages
#'
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