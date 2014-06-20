# /usr/local/bin/Rscript

## Generate metadata for marmoset server
# Load packages
library("plyr")

# set mirror
options(repos=structure(c(CRAN="http://cran.r-project.org/")))

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

getmessage <- function(pkg){
	tmp <- readLines(sprintf("/MRAN/www/diffs/RRT_%s.txt", dirtoget))
  tmp <- tmp[grep("\\+", tmp)]
  tmp <- gsub(".+/", "", tmp[grep("tar.gz", tmp)])
  tmp <- unlist(lapply(tmp, function(x) { bb = strsplit(x, "_")[[1]][[1]]; names(x) <- bb; as.list(x) }))
  tt <- tryCatch(tmp[[pkg]], error = function(e) e)
  if("error" %in% class(tt)){ NULL } else {
    sprintf("New package version %s", tt)
  }
}

### define things
dirs <- list.files("/MRAN/RRT/.zfs/snapshot")
dirtoget <- dirs[length(dirs)]
pkgdirs <- list.files(file.path("/MRAN/RRT/.zfs/snapshot", dirtoget))
# justpkgnames <- sapply(pkgdirs, function(x) strsplit(x, "_")[[1]][[1]], USE.NAMES=FALSE)

#### get metadata from CRAN
availpkgs <- available.packages(contrib.url(getOption("repos"), "source"))
extractpkginfo <- function(pkg){
  tmp <- tryCatch(availpkgs[pkg,], error=function(e) e)
  if("error" %in% class(tmp) || is.null(tmp)){ NULL } else {
    tmp <- tmp[!names(tmp) %in% "Repository"]
    list(package=tmp[["Package"]], description=as.list(tmp[!is.na(tmp)]))
  }
}
aslist <- lapply(pkgdirs, extractpkginfo)
aslist <- compact(aslist)

### add more metadata to the json (as list), then write again to json
#### get archive data
mranurl <- 'http://marmoset.revolutionanalytics.com/snapshots/%s/%s/%s'
archiveurls <- lapply(pkgdirs, function(z){
  # tmp <- sprintf(mranurl, dirtoget, z, list.files(file.path("/MRAN/RRT/.zfs/snapshot", dirtoget, z)))
  tmp <- list.files(file.path("/MRAN/RRT/.zfs/snapshot", dirtoget, z))
  names(tmp) <-
    vapply(list.files(file.path("/MRAN/RRT/.zfs/snapshot", dirtoget, z)), function(x) gsub(sprintf("%s_|.tar.gz", z), "", x), "", USE.NAMES=FALSE)
  tmp
})
names(archiveurls) <- pkgdirs

allpkgs <- lapply(aslist, function(x){
  list(package = x$package,
    description = x$description,
    snapshotId = dirtoget,
    snapshotDate = dirtoget,
    snapshotDiffId = sprintf("RRT_%s.txt", dirtoget),
    compatibitlityCheck = NULL,
    message = getmessage(x$package),
    source = list(baseurl = sprintf("http://marmoset.revolutionanalytics.com/snapshots/%s/%s/", dirtoget, x$package),
      ver = as.list(archiveurls[[x$package]])),
    windows = current_windows(x$package, x$description$Version),
    osx = current_osx(x$package, x$description$Version)
  )
})

### Convert to JSON
library("jsonlite")
jsonallpkgs <- lapply(allpkgs, function(x) toJSON(x, auto_unbox = TRUE))

### Write JSON to disk
# now <- Sys.Date()
dircreate <- sprintf("/MRAN/www/metadata/logs/%s", dirtoget)
dir.create(dircreate)
for(i in seq_along(jsonallpkgs)){
  path <- sprintf("/MRAN/www/metadata/logs/%s/%s.json", dirtoget, allpkgs[[i]]$package)
  on.exit(close(path))
  writeLines(jsonallpkgs[[i]], path)
}
