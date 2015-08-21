
stopIfInvalidDate <- function(snapshotDate){
  if(missing(snapshotDate) || is.null(snapshotDate))
    stop("You have to specify a snapshotDate", call. = FALSE)
  if(!grepl("^\\d{4}-\\d{2}-\\d{2}$", snapshotDate))
    stop("snapshotDate must be a valid date using format YYYY-MM-DD", call. = FALSE)
  if(as.Date(snapshotDate) < as.Date("2014-09-17"))
    stop("Snapshots are only available after 2014-09-17", call. = FALSE)
  if(as.Date(snapshotDate) > Sys.Date())
    stop("snapshotDate can not be in the future!", call. = FALSE)
  
}

testHttps <- function(https){
  tf = tempfile()
  dir.create(tf)
  on.exit(unlink(tf))
  testpkg = "memoise"
  repos <- paste0(https, "snapshot/2014-09-12/")
  tryCatch(suppressWarnings(utils::install.packages(testpkg, lib = tf, 
                                   repos = repos ,
                                   dependencies = FALSE, 
                                   type = "source",
                                   quiet = TRUE)))
  if(testpkg %in% installed.packages(lib.loc = tf)[, "Package"]) {
    TRUE
  } else {
    FALSE
  }
}

mranUrlDefault <- function(){
  http = "http://mran.revolutionanalytics.com/"
  https = gsub("http://", replacement = "https://", http)
  if(getRversion() >= "3.2.2") {
    if(testHttps(https)) https else  http
  } else {
    http
  }
}

getDownloadOption <- function(){
  getOption("download.file.method")
}


isHttpsUrl <- function(url){
  grepl("^https://", url)
}

# setDownloadOption <- function(mranUrl){
#   
#   download.method <- switch(
#     .Platform$OS.type,
#     windows = "wininet",
#     unix    = if(capabilities("libcurl")) "libcurl" else "curl"
#   )
#   url.method <- switch(
#     .Platform$OS.type,
#     windows = "wininet",
#     unix    = if(capabilities("libcurl")) "libcurl" else "internal"
#   )
#   
#   options(download.file.method = download.method, 
#           url.method = url.method)
# }
# 
# resetDownloadOption <- function(opts){
#   options(opts)
# }


#  ------------------------------------------------------------------------

mranUrl <- function(){
  url <- getOption("checkpoint.mranUrl")
  url <- if(is.null(url)) mranUrlDefault() else url
  url <- gsub("snapshot/*$", "", url)
  if(substring(url, nchar(url)) != "/") url <- paste0(url, "/")
  
  paste0(url, "snapshot/")
}



setCheckpointUrl <- function(url){
  options("checkpoint.mranUrl" = url)
}


#  ------------------------------------------------------------------------



#' Read list of available snapshot dates from MRAN url.
#' 
#' @param mranRootUrl URL of MRAN root, e.g. \code{"http://mran.revolutionanalytics.com/snapshot/"}
#' 
#' @export
getValidSnapshots <- function(mranRootUrl = mranUrl()){
  text <- tryCatch(readLines(mranRootUrl, warn = TRUE), error=function(e)e)
  if(inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", text$message))
  }
  ptn <- "\\d{4}-\\d{2}-\\d{2}"
  idx <- grep(ptn, text)
  gsub(sprintf("^<a href=.*?>(%s).*?</a>.*$", ptn), "\\1", text[idx])
}


#  ------------------------------------------------------------------------

is.404 <- function(mran, method = "libcurl"){
  con <- url(mran, method = method)
  on.exit(close(con))
  x <- tryCatch(readLines(con, warn = FALSE), 
                error = function(e)e)
  if(inherits(x, "error")) return(TRUE)
  ptn <- "404.*Not Found"
  any(grepl(ptn, x))}

getSnapshotUrl <- function(snapshotDate, mranRootUrl = mranUrl()){
  
  if(is.404(mranRootUrl)){
    warning("Unable to reach MRAN root at ", mranRootUrl, call. = FALSE)
  }
  
  snapshot.url = paste(gsub("/$", "", mranRootUrl), snapshotDate, sep = "/")
  if(is.404(snapshot.url)){
    warning("Unable to find snapshot on MRAN at ", snapshot.url, call. = FALSE)
  }
  snapshot.url
}

