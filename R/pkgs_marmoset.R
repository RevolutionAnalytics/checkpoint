#' Install from Marmoset server
#' 
#' @export
#' @param date Date as "year-month-day" (YY-MM-DD)
#' @param outdir Output directory
#' @param pkgs Packages to install with version numbers, e.g. plyr_1.8.1
#' @examples \dontrun{
#' # By default installs most recent version
#' pkgs_marmoset(date='2014-06-19', pkgs=c("plyr","ggplot2"), outdir="~/marmoset_snaps/stuff/")
#' 
#' 
#' pkgs_marmoset(date='2014-06-19', pkgs=c("plyr_1.8.1","ggplot2_1.0.0"), outdir="~/marmoset_snaps/stuff/")
#' pkgs_marmoset(date='2014-06-19', pkgs="rgbif_0.6.2", outdir="~/marmoset_snaps/stuff/")
#' }

pkgs_marmoset <- function(date=NULL, pkgs=NULL, outdir=NULL)
{
  if(is.null(outdir)) stop("You must specify a directory to download packages to")
  if(is.null(pkgs)) stop("You must specify one or more packages to get")
  
  # get available snapshots
#   availcmd <- sprintf('ssh %s@marmoset.revolutionanalytics.com "ls /MRAN/RRT/.zfs/snapshot/"', user)
#   availsnaps <- system(availcmd, intern = TRUE)
  availsnaps <- suppressMessages(marmoset_snaps())
  
  if(is.null(date)) date <- Sys.Date()
  snapshots <- grep(date, availsnaps, value = TRUE)
  if(length(snapshots) > 1){
    print(data.frame(snapshots))
    message("\nMore than one snapshot matching your date found \n",
            "Enter rownumber of snapshot (other inputs will return 'NA'):\n")
    take <- scan(n = 1, quiet = TRUE, what = 'raw')
    if(is.na(take)){ message("No snapshot found or you didn't select one") }
    snapshot_use <- snapshots[as.numeric(take)]
  }
  
  # parse versions from pkgs
  foo <- function(x){
    vers <- marmoset_pkg_avail(snapshot=snapshot_use, package=x[[1]])
    names(vers) <- as.numeric(gsub("\\.", "", vers))
    verssorted <- sort(vers)
    pkgver <- tryCatch(x[[2]], error=function(e) e)
    if('error' %in% class(pkgver)) { 
      pkgveruse <- unname(verssorted[length(verssorted)])
    } else {
      pkgveruse <- if(pkgver %in% vers) pkgver else unname(verssorted[length(verssorted)])
    }
    sprintf("%s/%s_%s.tar.gz", x[[1]], x[[1]], pkgveruse)
  }

  pkgs <- lapply(pkgs, function(x) strsplit(x, "_")[[1]])
  pkgpaths <- sapply(pkgs, foo)
  
  tmppkgsfileloc <- tempfile()
  cat(pkgpaths, file = tmppkgsfileloc, sep = "\n")
  cmd <- sprintf('rsync -rtv --files-from=%s sckott@marmoset.revolutionanalytics.com:/MRAN/RRT/.zfs/snapshot/%s %s', 
                 tmppkgsfileloc, snapshot_use, outdir)
  setwd(outdir)
  mvcmd <- sprintf("mv %s .", paste(pkgpaths, collapse = " "))
  rmcmd <- sprintf("rm -rf %s", paste(sapply(pkgpaths, function(x) strsplit(x, "/")[[1]][[1]], USE.NAMES = FALSE), collapse = " "))
  system(cmd)
  system(mvcmd)
  system(rmcmd)
}