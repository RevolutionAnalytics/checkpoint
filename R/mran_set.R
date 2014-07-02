#' Set the MRAN snapshot to get packages from.
#'
#' @import httr RJSONIO
#' @export
#'
#' @param snapshot A MRAN snapshot. Defaults to most recent snapshot
#'
#' @examples \dontrun{
#' mran_set()
#' mran_set(snapshot="2014-07-01_2000")
#' }

mran_set <- function(snapshot=NULL, repo=getwd())
{
  if(is.null(snapshot)){
    gg <- suppressMessages(mran_snaps())
    snapshot <- gg[length(gg)]
  }

  url <- sprintf("http://marmoset.revolutionanalytics.com/snapshots/%s/", snapshot)
  res <- GET(url)
  if(res$status_code >= 300)
    stop(sprintf("%s - snapshot not found, you don't have an internet connection, or other error...", res$status_code))

  # check for folder, and rrt folder inside
  check4repo(repo, TRUE)
  lib <- rrt_libpath(repo)
  check4rrt(repo, lib, TRUE)
  
  # write MRAN snapshot id
  writeManifest(repo, lib, packs = NULL, repoid = digest(repo), snapshot = snapshot)
}

# #' Write MRAN snapshot ID to internal manifest file.
# #'
# #' @export
# #' @param repository Repository root path
# #' @param librar Library to install packages in
# #' @param snapshot MRAN snapshot ID
# #'
# #' @keywords internal
# #' @return Writes a MRAN snapshot ID line to the RRT manifest file ("rrt_manifest.yml")
# writemransnap <- function(rr, librar, snapshot)
# {
#   mfile <- file.path(rr, "rrt", "rrt_manifest.yml")
#   datecheck <- check_snapshot(x=mfile)
#   rrtsnapshot <- sprintf("RRT_snapshotID: %s", getOption("RRT_snapshotID", ""))  
#   cat(info, file = infofile, sep = "\n")
# }
# 
# check_snapshot <- function(){
#   if(file.exists(x)){
#     info <- readLines(x)
#     dcline <- grep("RRT_snapshotID", info, value = TRUE)
#     if(length(dcline) > 0) dcline else NULL
#   } else { NULL }
# }