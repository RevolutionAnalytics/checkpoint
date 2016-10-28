
stopIfInvalidDate <- function(snapshotDate, verbose = TRUE){
  if(missing(snapshotDate) || is.null(snapshotDate))
    stop("You have to specify a snapshotDate", call. = FALSE)
  if(!grepl("^\\d{4}-\\d{2}-\\d{2}$", snapshotDate))
    stop("snapshotDate must be a valid date using format YYYY-MM-DD", call. = FALSE)
  if(as.Date(snapshotDate) < as.Date("2014-09-17"))
    stop("Snapshots are only available after 2014-09-17", call. = FALSE)
  if(as.Date(snapshotDate) > Sys.Date())
    stop("snapshotDate can not be in the future!", call. = FALSE)
  
  
  validSnapshots <- tryCatch(as.Date(getValidSnapshots()), error=function(e)e)
  if(inherits(validSnapshots, "error")){
    mssg(verbose, "Unable to connect to MRAN. Skipping some date validations.") 
  } else {
    if(!as.Date(snapshotDate) %in% validSnapshots) {
      i <- findInterval(as.Date(snapshotDate), validSnapshots)
      suggestions <- validSnapshots[c(i, i+1)]
      stop(sprintf("Snapshot does not exist on MRAN. Try %s or %s.", validSnapshots[i], validSnapshots[i+1]))
    }
  }
  
  
}

# testHttps <- function(https){
#   tf = tempfile()
#   dir.create(tf)
#   on.exit(unlink(tf))
#   testpkg = "memoise"
#   repos <- paste0(https, "snapshot/2014-09-12/")
#   tryCatch(suppressWarnings(utils::install.packages(testpkg, lib = tf, 
#                                    repos = repos ,
#                                    dependencies = FALSE, 
#                                    type = "source",
#                                    quiet = TRUE)))
#   if(testpkg %in% installed.packages(lib.loc = tf)[, "Package"]) {
#     TRUE
#   } else {
#     FALSE
#   }
# }

mranUrlDefault <- function(){
  http = "http://mran.microsoft.com/"
  https = gsub("http://", replacement = "https://", http)
  if(getRversion() >= "3.2.0" && httpsSupported()) {
    https 
    #     Attempt to connect
    #     if unable to connect, stop with warning
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


tryUrl <- function(url){
  con <- suppressWarnings(tryCatch(url(url), error = function(e)e))
  msg <- paste0(
    "Invalid value for mranRootUrl.\n", 
    "Ensure you use the correct http://,  https:// or file:/// prefix."
  )
  if(inherits(con, "error")) {
    stop(msg, call. = FALSE)
  }
  con
}

#' Read list of available snapshot dates from MRAN url.
#' 
#' @param mranRootUrl URL of MRAN root, e.g. \code{"http://mran.microsoft.com/snapshot/"} or \code{"file:///local/path"}
#' 
#' @export
getValidSnapshots <- function(mranRootUrl = mranUrl()){
  con <- tryUrl(mranRootUrl)
  on.exit(close(con))
  text <- if (inherits(con, "file")) {
    dir(summary(con)$description)
  } else {
    suppressWarnings(tryCatch(readLines(con, warn = TRUE), error = function(e) e))
  }
  if (inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", 
                 text$message))
  }
  ptn <- "\\d{4}-\\d{2}-\\d{2}"
  idx <- grep(ptn, text)
  gsub(sprintf("^<a href=.*?>(%s).*?</a>.*$", ptn), 
       "\\1", text[idx])
}


#  ------------------------------------------------------------------------

libcurl <- function() isTRUE(unname(capabilities("libcurl")))

url <- function(url){
  if(getRversion() >= "3.2.0"){
    method <- switch(.Platform$OS.type, 
                     "unix" = if(libcurl()) "libcurl" else "default",
                     "windows" = "wininet",
                     "default"
    )
    base::url(url, method = method)
  } else {
    base::url(url)
  }
}

httpsSupported <- function(mran = "https://mran.microsoft.com/snapshot"){
  tf <- tempfile()
  on.exit(unlink(tf))
  pdb <- suppressWarnings({
    testfile <- paste0(mran, if(grepl("snapshot$", mran)) 
      "/2015-09-01/src/contrib/checkTimings.html" else
        "/src/contrib/checkTimings.html"
    )
    try(download.file(url = testfile, destfile = tf, mode = "w",
                      cacheOK = FALSE, quiet = TRUE),
        silent = TRUE
    )
  })
  if(inherits(pdb, "error")) return(FALSE)
  con <- suppressWarnings({
    tryCatch(url(mran), 
             error = function(e)e)
  }) 
  if(inherits(con, "error")) return(FALSE)
  on.exit(close(con))
  x <- suppressWarnings(
    tryCatch(readLines(con, warn = FALSE), 
             error = function(e)e)
  )
  if(!inherits(x, "error")) return(TRUE)
  if(x$message == "cannot open the connection") return(FALSE)
  warning(x$message)
  FALSE
}


is.404 <- function(mran, warn = TRUE){
  if(isHttpsUrl(mran) && !httpsSupported(mran)) {
    if(warn) warning("It seems that https URLs are not supported on this platform")
    return(TRUE)
  }
  con <- tryUrl(mran)
  on.exit(close(con))
  if(inherits(con, "file")) {
    dirPath <- summary(con)$description
    !dir.exists(dirPath)
  } else {
    x <- suppressWarnings(tryCatch(readLines(con, warn = FALSE), 
                                   error = function(e) e))
    if (inherits(x, "error")) 
      return(TRUE)
    ptn <- "404.*Not Found"
    any(grepl(ptn, x))
  }
}

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

