#' Get available snapshots from Marmoset
#' 
#' @import httr XML
#' @export
#' @examples \dontrun{
#' marmoset_snaps()
#' }

marmoset_snaps <- function(){
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

#' Get available diffs from Marmoset
#' 
#' @import httr XML
#' @export
#' @param diff Optional. (character) A diff date-time stamp of a Marmoset diff.
#' @examples \dontrun{
#' marmoset_diffs()
#' 
#' # An individual diff
#' marmoset_diffs(diff="2014-06-19_0136")
#' }

marmoset_diffs <- function(diff=NULL)
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

#' Get available package level metadata from Marmoset
#' 
#' @import httr RJSONIO
#' @export
#' @param snapshot A Marmoset snapshot. Defaults to most recent snapshot
#' @param package Required. A package name
#' @examples \dontrun{
#' marmoset_pkg_metadata(package="plyr")
#' }

marmoset_pkg_metadata <- function(snapshot=NULL, package)
{
  if(is.null(snapshot)) snapshot <- suppressMessages(marmoset_snaps()[1])
  snapshot <- "2014-06-17_2300" # forcing to only available metadata for now
  
  url <- sprintf("http://marmoset.revolutionanalytics.com/metadata/logs/%s/%s.json", snapshot, package)
  res <- GET(url)
  if(res$status_code > 202) 
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  RJSONIO::fromJSON(text, simplifyWithNames = FALSE)  
}


#' Get available package versions from Marmoset
#' 
#' @import httr RJSONIO
#' @export
#' @param snapshot A Marmoset snapshot. Defaults to most recent snapshot
#' @param package Required. A package name
#' @examples \dontrun{
#' marmoset_pkg_avail(snapshot="2014-06-19_0136", package="plyr")
#' }

marmoset_pkg_avail <- function(snapshot=NULL, package)
{
  if(is.null(snapshot)){
    gg <- suppressMessages(marmoset_snaps())
    snapshot <- gg[length(gg)]
  }
  
  url <- sprintf("http://marmoset.revolutionanalytics.com/snapshots/%s/%s/", snapshot, package)
  res <- GET(url)
  if(res$status_code > 202) 
    stop(sprintf("%s - You don't have an internet connection, or other error...", res$status_code))
  text <- content(res, as = "text")
  vers <- xpathSApply(htmlParse(text), "//a", xmlValue)[-1]
  vers <- gsub(sprintf(".tar.gz|%s_", package), "", vers)
  return( vers )
}