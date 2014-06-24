#' Get available snapshots from MRAN
#'
#' @import httr XML
#' @export
#' @examples \dontrun{
#' mran_snaps()
#' }

mran_snaps <- function(){
  url <- "http://marmoset.revolutionanalytics.com/snapshots/"
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  snaps <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  snaps <- gsub("/", "", snaps)
  message("Dates and times are in GMT")
  return( snaps )
}

#' Get available diffs from MRAN
#'
#' @import httr XML
#' @export
#' @param diff Optional. (character) A diff date-time stamp of a MRAN diff.
#' @examples \dontrun{
#' mran_diffs()
#'
#' # An individual diff
#' mran_diffs(diff="2014-06-19_0136")
#'
#' diffs <- mran_diffs()
#' mran_diffs(diffs[length(diffs)-1])
#' }

mran_diffs <- function(diff=NULL)
{
  if(is.null(diff)){
    url <- "http://marmoset.revolutionanalytics.com/diffs/"
  } else { url <- sprintf('http://marmoset.revolutionanalytics.com/diffs/RRT_%s.txt', diff) }
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")

  message("Dates and times are in GMT")

  if(is.null(diff)){
    diffs <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
    diffs <- gsub("RRT_|.txt", "", diffs)
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
#' mran_pkg_metadata(package="plyr")
#' }

mran_pkg_metadata <- function(package, snapshot=NULL)
{
  if(is.null(snapshot)){
    gg <- suppressMessages(mran_snaps())
    snapshot <- gg[length(gg)]
  }
#   snapshot <- "2014-06-17_2300" # forcing to only available metadata for now

  url <- sprintf("http://marmoset.revolutionanalytics.com/metadata/logs/%s/%s.json", snapshot, package)
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
#' @examples \dontrun{
#' mran_pkg_avail(snapshot="2014-06-19_0136", package="plyr")
#'
#' # Example of differences in available versions between snapshots for the package MPSEM
#' snaps <- mran_snaps()
#' mran_pkg_avail(snaps[length(snaps)-1], package="MPSEM")
#' mran_pkg_avail(snaps[length(snaps)-2], package="MPSEM")
#' }

mran_pkg_avail <- function(package, snapshot=NULL)
{
  if(is.null(snapshot)){
    gg <- suppressMessages(mran_snaps())
    snapshot <- gg[length(gg)]
  }

  url <- sprintf("http://marmoset.revolutionanalytics.com/snapshots/%s/%s/", snapshot, package)
  res <- GET(url)
  if(res$status_code > 202)
    stop(sprintf("%s - Package not found, you don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  vers <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  vers <- gsub(sprintf(".tar.gz|%s_", package), "", vers)
  return( vers )
}
