## Generate metadata for marmoset server

### some funcctions to use below
current_windows <- function(pkg){
  uri <- 'http://cran.r-project.org/bin/windows/contrib/%s/%s_1.8.1.zip'
  list(
    'R3.2' = sprintf(uri, 3.2, pkg),
    'R3.1' = sprintf(uri, 3.1, pkg),
    'R3.0' = sprintf(uri, 3.0, pkg)
  )
}

current_osx <- function(pkg){
  uri <- 'http://cran.r-project.org/bin/macosx/contrib/%s/%s_1.8.1.zip'
  list(
    'R3.1' = sprintf(uri, 3.1, pkg),
    'R3.0' = sprintf(uri, 3.0, pkg),
    'R3.1_mavericks' = sprintf('http://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/%s_1.8.1.tgz', pkg)
  )
}

getarchive <- function(repos = getOption("repos")){
  con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", repos), "rb"))
  on.exit(close(con))
  readRDS(con)
}

archive_source <- function(pkg, archive){
  archivelist <- archive[[pkg]]
  nn <- rownames(archivelist)
  nn <- gsub(sprintf("%s/|\\.tar\\.gz", pkg), "", nn)
  tmp <- pkgurls(nn)
  names(tmp) <- gsub(sprintf("%s_|\\.tar\\.gz", pkg, pkg), "", nn)
  as.list(tmp)
}

### define things
pkgs <- c("plyr","reshape2","ggplot2")
pkgPath <- "~/testingrtt2"
dir.create(pkgPath)

### download packages
download.packages(pkgs, destdir = pkgPath, type = 'source')

#### generate metadata PACKAGES and PACKAGES.gz files
tools::write_PACKAGES(pkgPath)

### convert to JSON
library("jsonlite")
pkgsinfo <- readLines(file.path(pkgPath, "PACKAGES"))
pkgstarts <- grep("Package: ", pkgsinfo)
pkgends <- which(vapply(pkgsinfo, nchar, 1, USE.NAMES = FALSE)==0)
out <- list()
for(i in seq_along(pkgstarts)){
  tmp <- pkgsinfo[ pkgstarts[i] : pkgends[i]-1 ]
  tmp <- tmp[!vapply(tmp, nchar, 1)==0]
  out[[i]] <- tmp
}
extractpkginfo <- function(x){
  res <- do.call(c, lapply(x, function(y){
    res <- strsplit(y, ":\\s")[[1]]
    ll <- list(res[2])
    names(ll) <- res[1]
    ll
  }))
  list(package=res$Package, description=res)
}
# 
aslist <- lapply(out, extractpkginfo)

### add more metadata to the json (as list), then write again to json
#### get archive data
archive <- getarchive()

#### load RRT
library("RRT")

allpkgs <- lapply(aslist, function(x){
  list(package=x$package,
    description=x$description,
    snapshotId = 123456, 
    snapshotDate = "2014-06-05", 
    snapshotDiffId = "19293838-12312323",
    compatibitlityCheck = NULL,
    source = archive_source(x$package, archive),
    windows = current_windows(x$package),
    osx = current_osx(x$package)
  )
})

# pkgsinfo <- gsub("\\s+", " ", paste(pkgsinfo, sep = "", collapse = " "))
# json <- toJSON(extractpkginfo(out[[2]]), pretty = TRUE)
json <- toJSON(allpkgs, auto_unbox = TRUE)
# cat(json)

### write JSON to disk
mran_json <- file.path(pkgPath, sprintf("mran_json_%s.json", Sys.Date()))
writeLines(json, mran_json)
