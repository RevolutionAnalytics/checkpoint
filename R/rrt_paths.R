repo.exists <- function(repo){
  #   mssg(verbose, "Checking to make sure repository exists...")
  file.exists(repo)
}


#' Returns folder paths for rrt library and src for repo.
#'
#' @param repo Repo path
#' @param type Either "lib" or "src"
#' @export
#' @family Repo path
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



#  ------------------------------------------------------------------------



# #' Returns library path string for repo.
# #'
# #' @param repo Repo
# #' @export
# #' @family paths
# rrt_libpath <- function(repo){
#   normalizePath(
#     file.path(repo, "rrt", "lib", R.version$platform, base::getRversion()),
#     mustWork=FALSE)
# }
#
# #' Returns source path string for repo.
# #'
# #' @param repo Repo
# #' @export
# #' @family paths
# rrt_srcpath <- function(repo){
#   normalizePath(
#     file.path(rrt_libpath(repo), "src/contrib"),
#     mustWork=FALSE)
# }
#
#
#
#
#
#
# #' Create folder for repo library path
# #'
# #' @param repo Repo
# #' @export
# #' @family paths
# create_libpath_folder <- function(repo){
#   pth <- rrt_libpath(repo)
#   if(!file.exists(pth)) dir.create(pth, recursive=TRUE)
# }
#
# #' Create folder for repo package source path
# #'
# #' @param repo Repo
# #' @export
# #' @family paths
# create_srcpath_folder <- function(repo){
#   pth <- rrt_srcpath(repo)
#   if(!file.exists(pth)) dir.create(pth, recursive=TRUE)
# }
