
checkpointPath <- function(snapshotDate, checkpointLocation = "~/", 
                           type = c("lib", "src", "snapshot", "root", "base")){
  rootPath <- normalizePath(
    file.path(checkpointLocation, ".checkpoint"), 
    mustWork = FALSE)
  type <- match.arg(type)
  if(type == "base") return(
    normalizePath(
      file.path(rootPath, paste0("R-", getRversion())),
    winslash = "/", mustWork = FALSE)
  )
  snapshotPath <- file.path(rootPath, snapshotDate)
  libPath <- file.path(snapshotPath, "lib", R.version$platform, base::getRversion())
  srcPath <- file.path(libPath, "src/contrib")
  normalizePath(
    switch(
      type,
      root  = rootPath,
      lib   = libPath,
      src   = srcPath,
      snapshot = snapshotPath
    ),
    winslash = "/",
    mustWork = FALSE)}

createFolders <- function(snapshotDate, checkpointLocation = "~/"){
  paths <- sapply(c("root", "lib", "src"), checkpointPath, 
                  snapshotDate = snapshotDate,
                  checkpointLocation = checkpointLocation)
  sapply(paths, function(x) if(!file.exists(x)) dir.create(x, recursive=TRUE, showWarnings = FALSE))
  all(file.exists(paths))
}
