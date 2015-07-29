testHttps <- function(https){
  tf = tempfile()
  dir.create(tf)
  on.exit(unlink(tf))
  testpkg = "memoise"
  tryCatch(utils::install.packages(testpkg, lib = tf, repos = paste0(https, "snapshot/2014-09-12/") ,
                                   dependencies = FALSE, 
                                   type = "source",
                                   quiet = TRUE))
  if(require(testpkg, character.only = TRUE, lib.loc = tf, quietly = TRUE)) {
    on.exit(detach(paste0("package:", testpkg), unload = TRUE, character.only = TRUE), add = TRUE)
    TRUE
  } else {
    options(download.file.method = download.file.method)
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

setDownloadOption <- function(){
  is.recent  = getRversion() >= "3.2.0"
  is.unix = .Platform$OS.type == "unix"
  is.os.x = length(grep(pattern = "darwin", R.version$os)) > 0
  is.win = .Platform$OS.type == "windows"
  
  
  method <- switch(
    2 * is.recent + is.unix + 1,
    # 1 - old & win
    {
      utils::setInternet2(TRUE)
      "wininet"
    },
    # 2 - old & unix
    {
      if(is.os.x) "curl" else "wget"
    },
    # 3 - new and win
    {
      "wininet"
    },
    # 4 - new and unix
    {
      if(capabilities("libcurl")) "libcurl" else "wget"
    }
  )
  
  options(download.file.method = method)
  options(url.method = method)
}




#  ------------------------------------------------------------------------

mranUrl <- function(){
  url <- getOption("checkpoint.mranUrl")
  url <- if(is.null(url)) mranUrlDefault() else url
  url <- gsub("snapshot/*$", "", url)
  if(substring(url, nchar(url)) != "/") url <- paste0(url, "/")
  
  download.file.method = getDownloadOption()
  setDownloadOption()
  
  paste0(url, "snapshot/")
}



setCheckpointUrl <- function(url){
  setDownloadOption()
  options("checkpoint.mranUrl" = url)
}


#  ------------------------------------------------------------------------



#' Read list of available snapshot dates from MRAN url.
#' 
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
getValidSnapshots <- function(url = mranUrl()){
  setDownloadOption()
  text <- tryCatch(suppressWarnings(read_xml(url, as_html = TRUE)), error=function(e)e)
  if(inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", text$message))
  }
  links <- xml_find_all(text, "//a")
  dates <- xml_text(links)
  idx <- grep("\\d{4}-\\d{2}-\\d{2}/", dates)
  gsub("/$", "", dates[idx])
}
