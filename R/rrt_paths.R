repo.exists <- function(repo){
  #   mssg(verbose, "Checking to make sure repository exists...")
  file.exists(repo)
}



rrtPath <- function(repo, type = c("lib", "src", "manifest", "rootdir", "rootfile")){
  type <- match.arg(type)
  if(!missing("repo")){
    libPath <- normalizePath(
      file.path(repo, "rrt", "lib", R.version$platform, base::getRversion()),
      mustWork = FALSE)
    srcPath <- normalizePath(
      file.path(libPath, "src/contrib"),
      mustWork = FALSE)
    manifest <- normalizePath(
      file.path(repo, "rrt", "rrt_manifest.yml"),
      mustWork = FALSE)
  }
  rootdir <- normalizePath(
    file.path("~", ".rrt"),
    mustWork = FALSE)
  rootfile <- normalizePath(
    file.path(rootdir, "rrt.txt"),
    mustWork = FALSE)
  switch(type,
         lib      = libPath,
         src      = srcPath,
         manifest = manifest,
         rootdir  = rootdir,
         rootfile = rootfile
         )
}


createRepoFolders <- function(repo){
  repoPth <- normalizePath(repo, mustWork=FALSE)
  libPath <- rrtPath(repo, "lib")
  srcPath <- rrtPath(repo, "src")

  if(!file.exists(repoPth)) dir.create(repoPth, recursive=TRUE)
  if(!file.exists(libPath)) dir.create(libPath, recursive = TRUE)
  if(!file.exists(srcPath)) dir.create(srcPath, recursive = TRUE)
}

