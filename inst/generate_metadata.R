## Generate metadata for marmoset server

# Install packages 
# install.packages(c("devtools","jsonlite"), repos = "http://cran.us.r-project.org")
# library("devtools")
# devtools::install_github("RevolutionAnalytics/RRT")
# apt-get update
# wget http://cran.r-project.org/src/contrib/devtools_1.5.tar.gz
# wget http://cran.r-project.org/src/contrib/jsonlite_0.9.8.tar.gz
# R CMD INSTALL devtools_1.5.tar.gz
# R CMD INSTALL jsonlite_0.9.8.tar.gz
# 
# sudo su - \
# -c "R -e \"install.packages(c('devtools','jsonlite'), lib='/usr/lib/R/library/', repos='http://cran.us.r-project.org', dependencies=TRUE)\""
# 
# su - \
#   -c "R -e \"install.packages(c('devtools','jsonlite'), lib='/usr/lib/R/library/', repos='http://cran.us.r-project.org', dependencies=TRUE)\""

# get package urls
pkgurls <- function(pkgs)
{
  if(!any(is(pkgs) %in% c('data.frame','vector','list')))
    stop("input must be one of data.frame, vector, or lis")
  
  type <- class(pkgs)
  switch(type, 
         data.frame = geturlsdf(pkgs),
         character = geturlsdf(data.frame(do.call(rbind, strsplit(pkgs, "_")))))
}

geturlsdf <- function(x){
  # force column names
  names(x) <- c('pkg','ver')
  make_url <- function(x){
    sprintf('http://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz', x['pkg'], x['pkg'], x['ver'])
  }
  vapply(apply(x, 1, as.list), make_url, "")
}

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
  con <- gzcon(url("http://cran.r-project.org/src/contrib/Meta/archive.rds", "rb"))
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
#### NOTE: note sure we really need to do this...
# download.packages(pkgs, destdir = pkgPath, type = 'source')

#### generate metadata PACKAGES and PACKAGES.gz files
# tools::write_PACKAGES(pkgPath)
availpkgs <- available.packages(contrib.url(getOption("repos"), "source"))
extractpkginfo <- function(pkg){
  tmp <- availpkgs[pkg,]
  tmp <- tmp[!names(tmp) %in% "Repository"]
  list(package=tmp[["Package"]], description=as.list(tmp[!is.na(tmp)]))
}
aslist <- lapply(pkgs, extractpkginfo)

### add more metadata to the json (as list), then write again to json
#### get archive data
archive <- getarchive()

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
library("jsonlite", lib.loc = '/usr/local/lib/R/site-library')
json <- toJSON(allpkgs, auto_unbox = TRUE)

### Write JSON to disk
mran_json <- file.path(pkgPath, sprintf("mran_json_%s.json", Sys.Date()))
on.exit(close(mran_json))
writeLines(json, mran_json)