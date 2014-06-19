#' Install from Marmoset server
#' 
#' @export
#' @param date Date as "year-month-day" (YY-MM-DD)
#' @param user User name
#' @param outdir Output directory
#' @param pkgs Packages to install with version numbers, e.g. plyr_1.8.1
#' @examples \dontrun{
#' pkgs_marmoset(date='2014-06-17', user='sckott', outdir="~/marmoset_snaps/stuff/", 
#'    pkgs=c("plyr_1.8.1","ggplot2_1.0.0"))
#' pkgs_marmoset(date='2014-06-17', user='sckott', outdir="~/marmoset_snaps/stuff/", 
#'    pkgs="rgbif_0.6.2")
#' }

pkgs_marmoset <- function(date=NULL, user="sckott", outdir=NULL, pkgs=NULL)
{
  if(is.null(outdir)) stop("You must specify a directory to download packages to")
  if(is.null(pkgs)) stop("You must specify one or more packages to get")
  
  # get available snapshots
#   availcmd <- sprintf('ssh %s@marmoset.revolutionanalytics.com "ls /MRAN/RRT/.zfs/snapshot/"', user)
#   availsnaps <- system(availcmd, intern = TRUE)
  availsnaps <- marmoset_snaps()
  
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
  
  pkgstoinstall <- paste(pkgs, '.tar.gz', sep = "")
  tmppkgsfileloc <- tempfile()
  cat(pkgstoinstall, file = tmppkgsfileloc, sep = "\n")
  cmd <- sprintf('rsync -rtv %s@marmoset.revolutionanalytics.com:/MRAN/RRT/.zfs/snapshot/%s %s --files-from=%s', 
                 user, snapshot_use, outdir, tmppkgsfileloc)
  system(cmd)
}