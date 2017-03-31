

#' List checkpoint archives on disk.
#' 
#' @inheritParams checkpoint
#' 
#' @param full.names passed to [list.files()]
#' @export
#' @family checkpoint functions
#' @example inst/examples/example_remove.R
checkpointArchives <- function(checkpointLocation = "~/", full.names=FALSE){
  cpd <- checkpointPath("", checkpointLocation, type = "root")
  z <- list.files(path = cpd, 
                  pattern = "\\d{4}-\\d{2}-\\d{2}", 
                 recursive = FALSE,
                 include.dirs = TRUE,
                  full.names = full.names)
  if(full.names) z <- normalizePath(z, winslash = "/")
  z
}


#' Remove checkpoint archive from disk.
#' 
#' This function enables you to delete a snapshot archive folder from disk, thus releasing storage space. If you supply a single `snapshotDate`, then only this archive will be removed. You also have the option to remove a series of snapshots, including all snapshots before a given date, or all snapshots that have not been accessed since a given date.
#' 
#' @inheritParams checkpoint
#' @param allSinceSnapshot If `TRUE`, removes all snapshot archives since the `snapshotDate`
#' @param allUntilSnapshot If `TRUE`, removes all snapshot archives before the `snapshotDate`
#' @param notUsedSince If `TRUE`, removes all snapshot archives that have not been accessed since the `snapshotDate`. See [getAccessDate()]
#' @export
#' @family checkpoint functions
#' @seealso [getAccessDate()]
#' @example inst/examples/example_remove.R
checkpointRemove <- function(snapshotDate, checkpointLocation = "~/", 
                             allSinceSnapshot = FALSE, 
                             allUntilSnapshot = FALSE, 
                             notUsedSince = FALSE){
  if(!missing(snapshotDate) && !is.null(snapshotDate)){
    to_delete <- checkpointPath(snapshotDate, checkpointLocation, 
                                type = "snapshot")
  }
  if(allSinceSnapshot){
    archives <- checkpointArchives(checkpointLocation = checkpointLocation)
    archiveDates <- basename(archives)
    to_delete <- checkpointPath(archiveDates[archiveDates >= snapshotDate], 
                                checkpointLocation, type = "snapshot")
    
  }
  if(allUntilSnapshot){
    archives <- checkpointArchives(checkpointLocation = checkpointLocation)
    archiveDates <- basename(archives)
    to_delete <- checkpointPath(archiveDates[archiveDates <= snapshotDate], 
                                checkpointLocation, type = "snapshot")
    
  }
  if(notUsedSince){
    archiveDates <- getAccessDate(checkpointLocation = checkpointLocation)
    archiveDates <- archiveDates[!is.na(archiveDates)]
    archiveDates <- archiveDates[archiveDates >= snapshotDate]
    to_delete <- checkpointPath(basename(archiveDates), 
                                checkpointLocation, type = "snapshot")
    
  }
  if(length(to_delete) ==0 || !dir.exists(to_delete)) {
    message("no archives removed")
    invisible(NULL)
  } else {
    res <- unlink(to_delete, recursive = TRUE)
    if(res == 0) message("successfully removed archive")
    invisible(res)
  }
}


