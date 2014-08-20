# get mran_server url, if none found, defaults to global url: http://mran.revolutionanalytics.com
mran_server_url <- function(){
  x <- Sys.getenv('MRAN_SERVER')
  if(identical(x, "")) 'http://mran.revolutionanalytics.com' else x
}

#' Get available snapshots from MRAN
#'
#' @import httr XML
#' @export
#' @family mran
#' @param date (character) A date, in the format YYY-MM-DD
#' @examples \dontrun{
#' # List all available snapshots
#' mran_snaps()
#' # Get code for a single snapshot
#' mran_snaps(date='2014-08-04')
#' }

mran_snaps <- function(date=NULL){
  url <- file.path(mran_server_url(), 'snapshots/src')
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  snaps <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  snaps <- gsub("/", "", snaps)
  if(!is.null(date)) snaps <- snaps[grep(date, snaps)]
  message("Dates and times are in GMT")
  return( snaps )
}

#' Get available diffs from MRAN
#'
#' @import httr XML
#' @export
#' @family mran
#' @param diff Optional. (character) A diff date-time stamp of a MRAN diff.
#' @param which (character) One of src (for source packages) or bin (for binary packages).
#' @param os (character) Operating system. One of macosx, windows, or linux.
#' @examples \dontrun{
#' mran_diffs()
#' mran_diffs(which='bin')
#' mran_diffs(which='bin', os='windows')
#' mran_diffs(which='bin', os='linux')
#'
#' # An individual diff
#' mran_diffs(diff="2014-08-01_0500")
#'
#' diffs <- mran_diffs()
#' mran_diffs(diffs[length(diffs)-1])
#' }

mran_diffs <- function(diff=NULL, which='src', os='macosx')
{
  url <- mran_server_url()
  which <- match.arg(which, c('src','bin'))
  url <- if(which=='src') file.path(url, sprintf('diffs/%s/2014', which)) else file.path(url, sprintf('diffs/%s/%s/2014', which, os))
  if(!is.null(diff)){
    url <- sprintf('%s/%s.txt', url, diff)
  }
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")

  message("Dates and times are in GMT")

  if(is.null(diff)){
    diffs <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
    diffs <- gsub("RRT_|.txt", "", diffs)
    diffs <- diffs[!diffs %in% c('bin/','src/')]
    return( diffs )
  } else {
    cat(text)
  }
}

#' Get available package level metadata from MRAN
#'
#' @import httr RJSONIO
#' @export
#' @param package Required. A package name
#' @param snapshot An MRAN snapshot ('YYYY-MM-DD_TTTT') or a date ('YYYY-MM-DD'). Defaults to most 
#' recent snapshot.
#' @family mran
#' @examples \dontrun{
#' mran_pkg_metadata(package="plyr", snapshot="2014-08-04")
#' }

mran_pkg_metadata <- function(package, snapshot=NULL)
{
  snapshot <- snapshot_from_date(snapshot)
  url <- sprintf("%s/%s/%s.json", file.path(mran_server_url(), 'metadata/logs'), snapshot, package)
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - Package not found, you don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  RJSONIO::fromJSON(text, simplifyWithNames = FALSE)
}


#' Get available package versions from MRAN
#'
#' @import httr RJSONIO
#' @export
#' @param package (character) Required. A package name
#' @param snapshot (date) An MRAN snapshot ('YYYY-MM-DD_TTTT') or a date ('YYYY-MM-DD'). Defaults to most 
#' recent snapshot.
#' @param which (character) One of src or bin
#' @param os (character) Operating system. One of 'macosx', 'linux', or 'windows'

#' @family mran
#' @examples \dontrun{
#' mran_pkg_versions(snapshot="2014-07-14", package="plyr")
#' mran_pkg_versions(snapshot="2014-08-04", package="plyr", which="bin", os="windows")
#' }

mran_pkg_versions <- function(package, snapshot=NULL, which="src", os='macosx')
{
  snapshot <- snapshot_from_date(snapshot)
  md <- mran_pkg_metadata(package, snapshot)
  which <- match.arg(which, c('src','bin'))
  if(which=='src'){ names(md$source$ver) } else {
    tmp <- switch(os, macosx = md$osx[[1]], windows = md$windows[[1]])
    tmp2 <- strsplit(tmp, '/')[[1]]
    gsub("\\.zip|\\.tgz|[A-Za-z]+_", "", tmp2[length(tmp2)])
  }
}

snapshot_from_date <- function(x){
  if(is.null(x)){
    gg <- suppressMessages(mran_snaps())
    gg[length(gg)]
  } else {
    suppressMessages(mran_snaps(x))
  }
}
