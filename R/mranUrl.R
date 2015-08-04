
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
  tryCatch(utils::install.packages(testpkg, lib = tf, 
                                   repos = repos ,
                                   dependencies = FALSE, 
                                   type = "source",
                                   quiet = TRUE))
  if(testpkg %in% names(installed.packages(lib.loc = tf)[, "Package"])) {
    TRUE
  } else {
    FALSE
  }
}

mranUrlDefault <- function(){
  http = "http://mran.revolutionanalytics.com/"
  https = gsub("http://", replacement = "https://", http)
  if(getRversion() >= "3.2.0") {
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

setDownloadOption <- function(mranUrl){
  
#   is.recent  = getRversion() >= "3.2.2"
#   is.unix = .Platform$OS.type == "unix"
#   is.os.x = length(grep(pattern = "darwin", R.version$os)) > 0
#   is.win = .Platform$OS.type == "windows"
  
  method <- if(isHttpsUrl(mranUrl)){
    switch(.Platform$OS.type,
           windows = "wininet",
           unix    = if(capabilities("libcurl")) "libcurl" else "wget"
    )
  } else {
    switch(.Platform$OS.type,
           windows = {utils::setInternet2(TRUE); "wininet"},
           unix    = if(is.os.x) "curl" else "wget"
    )
  }
  
  options(download.file.method = method, url.method = method)
}

resetDownloadOption <- function(opts){
  options(opts)
}


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
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
getValidSnapshots <- function(mranRootUrl = mranUrl()){
  opts <- setDownloadOption(mranRootUrl)
  on.exit(resetDownloadOption(opts))
  text <- tryCatch(suppressWarnings(read_xml(mranRootUrl, as_html = TRUE)), error=function(e)e)
  if(inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", text$message))
  }
  links <- xml_find_all(text, "//a")
  dates <- xml_text(links)
  idx <- grep("\\d{4}-\\d{2}-\\d{2}/", dates)
  gsub("/$", "", dates[idx])
}


#  ------------------------------------------------------------------------

getSnapshotUrl <- function(snapshotDate, mranRootUrl = mranUrl()){
  
  opts <- setDownloadOption(mranRootUrl)
  on.exit(resetDownloadOption(opts))
  mran.root = url(mranRootUrl)
  snapshot.url = paste(gsub("/$", "", mranRootUrl), snapshotDate, sep = "/")
  on.exit(close(mran.root))
  res <- tryCatch(
    suppressWarnings(readLines(mran.root)),
    error = function(e) e
  )
  if(inherits(res, "error")) {
    warning("Unable to reach MRAN root at ", mranRootUrl, call. = FALSE)
    return(snapshot.url)
  }
  
  con = url(snapshot.url)
  on.exit(close(con), add = TRUE)
  res <- tryCatch(
    suppressWarnings(readLines(con)),
    error = function(e) e
  )
  if(inherits(res, "error")) {
    warning("Unable to find snapshot on MRAN at ", snapshot.url, call. = FALSE)
    return(snapshot.url)
  }
  snapshot.url
}

