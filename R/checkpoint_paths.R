checkpointPath <- function(snapshotDate, checkpointLocation = "~/.checkpoint", 
                           type = c("lib", "src", "snapshot", "root")){
  rootPath <- normalizePath(checkpointLocation, mustWork = FALSE)
  type <- match.arg(type)
  snpPath <- file.path(rootPath, snapshotDate)
  libPath <- file.path(snpPath, "lib", R.version$platform, base::getRversion())
  srcPath <- file.path(libPath, "src/contrib")
  normalizePath(
    switch(
      type,
      root  = rootPath,
      lib      = libPath,
      src      = srcPath,
      snapshot = snpPath
      ),
    winslash = "/",
    mustWork = FALSE)}

createFolders <- function(snapshotDate, checkpointLocation = "~/.checkpoint"){
  paths <- sapply(c("root", "lib", "src"), checkpointPath, 
                  snapshotDate = snapshotDate,
                  checkpointLocation = checkpointLocation)
  sapply(paths, function(x) if(!file.exists(x)) dir.create(x, recursive=TRUE))
  }
