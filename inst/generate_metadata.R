## Generate metadata for marmoset server

### some funcctions to use below
current_windows <- function(pkg, ver){
  uri <- 'http://cran.r-project.org/bin/windows/contrib/%s/%s_%s.zip'
  list(
    'R3.2' = sprintf(uri, "3.2", pkg, ver),
    'R3.1' = sprintf(uri, "3.1", pkg, ver),
    'R3.0' = sprintf(uri, "3.0", pkg, ver)
  )
}

current_osx <- function(pkg, ver){
  uri <- 'http://cran.r-project.org/bin/macosx/contrib/%s/%s_%s.tgz'
  list(
    'R3.1' = sprintf(uri, "3.1", pkg, ver),
    'R3.0' = sprintf(uri, "3.0", pkg, ver),
    'R3.1_mavericks' = sprintf('http://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/%s_%s.tgz', pkg, ver)
  )
}

getarchive <- function(repos = getOption("repos")){
  ## FIXME: this gzcon call throws error on linux, states unspported URL scheme
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
pkgPath <- "~/testingrtt3"
dir.create(pkgPath)

### download packages
download.packages(pkgs, destdir = pkgPath, type = 'source')

#### generate metadata PACKAGES and PACKAGES.gz files
# tools::write_PACKAGES(pkgPath)
availpkgs <- available.packages()
extractpkginfo <- function(pkg){
  tmp <- availpkgs[pkg,]
  tmp <- tmp[!names(tmp) %in% "Repository"]
  list(package=tmp[["Package"]], description=as.list(tmp[!is.na(tmp)]))
}
aslist <- lapply(pkgs, extractpkginfo)

### add more metadata to the json (as list), then write again to json
#### get archive data
archive <- getarchive()

#### load RRT to use pkgurls function
library("RRT")

allpkgs <- lapply(aslist, function(x){
  list(package=x$package,
    description=x$description,
    snapshotId = 123456, 
    snapshotDate = "2014-06-05", 
    snapshotDiffId = "19293838-12312323",
    compatibitlityCheck = NULL,
    source = archive_source(x$package, archive),
    windows = current_windows(x$package, x$description$Version),
    osx = current_osx(x$package, x$description$Version)
  )
})

### Convert to JSON
library("jsonlite")
json <- toJSON(allpkgs, auto_unbox = TRUE)

### Write JSON to disk
mran_json <- file.path(pkgPath, sprintf("mran_json_%s.json", Sys.Date()))
on.exit(close(mran_json))
writeLines(json, mran_json)
