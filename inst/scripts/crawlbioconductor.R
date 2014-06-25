#' Crawl R packages on Bioconductor
#' 
#' @import httr XML
#' 
#' @return a vector of owner/repo character strings
#' 
#' @examples \dontrun{
#' bioconductor_r()
#' }

bioconductor_r <- function(ver='2.14'){
  # get table itself
  taburl <- sprintf("http://bioconductor.org/packages/%s/bioc/", ver)
  tab <- readHTMLTable(taburl)
  
  # get urls for single pages
  biocpkgs <- all_group()
  biocpkgurls <- sprintf('http://bioconductor.org/packages/%s/bioc/html/%s.html', ver, biocpkgs)
  
  # scrape content for each pkg
  out <- list()
  for(i in seq_along(biocpkgurls)){
    out[[i]] <- scrapepkg(uri=biocpkgurls[[i]], pkg=biocpkgs[[i]])
  }
}

scrapepkg <- function(uri, pkg)
{
  res <- GET(uri)
  tt <- content(res, as="text")
  pp <- htmlParse(tt)
  links <- xpathApply(pp, "//a", xmlAttrs)
  htmlends <- grep("tar.gz|.tgz|.zip", unname(sapply(links, function(x) x['href'])), value = TRUE)
  htmlends <- gsub("\\.\\./", "", htmlends)
  tarlinks <- sprintf("http://bioconductor.org/packages/2.14/bioc/%s", htmlends)
  
  # get DESCRIPTION file contents
  desc <- readHTMLTable(pp, stringsAsFactors=FALSE)[[2]]
  entries <- desc[,2]
  nn <- gsub("\\s", "_", tolower(desc[,1]))
  names(entries) <- nn
  entries <- c(package=pkg, as.list(entries))
  
  # pkg download stats
  statslink <- sprintf("http://bioconductor.org/packages/stats/bioc/%s.html", pkg)
  tmpdf <- readHTMLTable(statslink, stringsAsFactors = FALSE)[[2]]
  statsdf <- data.frame(tmpdf[-1,], stringsAsFactors = FALSE)
  names(statsdf) <- as.character(as.list(tmpdf[1,]))
    
  # return data
  list(description=entries, downloads=tarlinks, stats=statsdf)
}