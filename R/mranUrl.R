mranUrlDefault <- "https://mran.revolutionanalytics.com/"

# mranUrl <- function(){
#   url <- getOption("checkpoint.mranUrl")
#   url <- if(is.null(url)) mranUrlDefault else url
#   url <- gsub("snapshot/*$", "", url)
#   if(substring(url, nchar(url)) != "/") url <- paste0(url, "/")
#   sprintf("%ssnapshot/", url)
# }

setCheckpointUrl <- function(url){
  setDownloadOption()
  options("checkpoint.mranUrl" = url)
}


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
