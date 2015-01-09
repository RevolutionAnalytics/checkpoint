checkpointPath <- function(snapshotDate, type = c("lib", "src", "snapshotDir", "rootDir", "rootfile")){
  rootDir <- normalizePath(
    file.path("~", ".checkpoint"),
    mustWork = FALSE)
  type <- match.arg(type)
  snapshotDir = file.path(rootDir, snapshotDate)
  libPath <-file.path(snapshotDir, "lib", R.version$platform, base::getRversion())
  srcPath <-  file.path(libPath, "src/contrib")
  rootFile <- file.path(rootDir, "checkpoint.txt")
  normalizePath(
    switch(
      type,
      lib      = libPath,
      src      = srcPath,
      rootDir  = rootDir,
      snapshotDir = snapshotDir,
      rootFile = rootFile),
    mustWork = FALSE)}

createFolders <- function(snapshotDate){
  paths =
    sapply(c("rootDir", "lib", "src"), checkpointPath, snapshotDate = snapshotDate)
  sapply(paths, function(x)  if(!file.exists(x)) dir.create(x, recursive=TRUE))}
