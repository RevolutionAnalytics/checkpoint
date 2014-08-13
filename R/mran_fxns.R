# get mran_server url, if none found, defaults to global url: http://mran.revolutionanalytics.com
mran_server_url <- function(){
  x <- Sys.getenv('MRAN_SERVER')
  if(identical(x, "")) 'http://mran.revolutionanalytics.com' else x
}

#' Get available snapshots from MRAN
#'
#' @import httr XML
#' @export
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
#' @param snapshot A MRAN snapshot. Defaults to most recent snapshot
#' @examples \dontrun{
#' mran_pkg_metadata(package="plyr", snapshot="2014-08-04")
#' }

mran_pkg_metadata <- function(package, snapshot=NULL)
{
  if(is.null(snapshot)){
    gg <- suppressMessages(mran_snaps())
    snapshot <- gg[length(gg)]
  } else { snapshot <- suppressMessages(mran_snaps(snapshot)) }

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
#' @param package Required. A package name
#' @param snapshot A MRAN snapshot. Defaults to most recent snapshot
#' @param which one of src or bin
#' @examples \dontrun{
#' mran_pkg_avail(snapshot="2014-07-14_0500", package="plyr")
#' mran_pkg_avail(snapshot="2014-06-19_0136", package="plyr", which="bin/windows/")
#'
#' # Example of differences in available versions between snapshots for the package MPSEM
#' snaps <- mran_snaps()
#' mran_pkg_avail(snaps[length(snaps)-1], package="MPSEM")
#' mran_pkg_avail(snaps[length(snaps)-2], package="MPSEM")
#' }

mran_pkg_avail <- function(package, snapshot=NULL, which="src")
{
  if(is.null(snapshot)){
    gg <- suppressMessages(mran_snaps())
    snapshot <- gg[length(gg)]
  }

  url <- sprintf("%s/%s/%s/", file.path(mran_server_url(), 'snapshots', which), snapshot, package)
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - Package not found, you don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  vers <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  vers <- gsub(sprintf(".tar.gz|%s_", package), "", vers)
  return( vers )
}
