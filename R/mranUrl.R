mranUrlDefault <- "http://mran.revolutionanalytics.com/"

mranUrl <- function(){
  url <- getOption("checkpoint.mranUrl")
  url <- if(is.null(url)) mranUrlDefault else url
  url <- gsub("snapshot/*$", "", url)
  if(substring(url, nchar(url)) != "/") url <- paste0(url, "/")
  sprintf("%ssnapshot/", url)
}

setCheckpointUrl <- function(url){
  options("checkpoint.mranUrl" = url)
}

