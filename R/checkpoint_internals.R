
setMranMirror <- function(
  snapshotDate, 
  snapshotUrl = checkpoint:::getSnapshotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(checkpointLocation, libPath){
  assign(".lib.loc", c(libPath, 
                       checkpointBasePkgs(checkpointLocation)), 
         envir = environment(.libPaths))
}


#' Undo the effect of checkpoint by resetting .libPath to user library location.
#' 
#' This is an experimental solution to the situation where a user no longer wants to work in the checkpointed environment. The function resets [.libPaths] to the environment variable `R_Libs_User`.
#' 
#' @param new The new user library location. Defaults to `Sys.getenv("R_Libs_User")`
#' 
#' @export
#' @family checkpoint functions
unCheckpoint <- function(new = Sys.getenv("R_Libs_User")){
  assign(".lib.loc", new, 
         envir = environment(.libPaths))
}


# Simple wrapper around `message()`, that only displays a message if verbose=TRUE
mssg <- function(verbose, ...) if(verbose) message(...)

correctR <- function(x) compareVersion(as.character(utils::packageVersion("base")), x) == 0


# Scans for R files in a folder and the first level subfolders.
anyRfiles <- function(path = "."){
  findRfiles <- function(path = "."){
    pattern <- "\\.[rR]$|\\.[rR]nw$|\\.[rR]md$|\\.[rR]pres$|\\.[rR]proj$"
    list.files(path = path, pattern = pattern, full.names = TRUE)
  }
  dirs <- list.dirs(path = path, recursive = FALSE)
  rfilesInDirs <- as.vector(unlist(sapply(dirs, findRfiles)))
  rfiles <- findRfiles(path = path)
  length(c(rfiles, rfilesInDirs)) > 0
}


# Use a simple heuristic to decide if the project looks like an R project.
validateProjectFolder <- function(project) {
  c1 <- normalizePath(project) == normalizePath("~/")
  c2 <- !anyRfiles(project)
  if(c1 && c2){
    message("This doesn't look like an R project directory.\n", 
            "Use forceProject = TRUE to force scanning"
    )
    answer = readline("Continue (y/n)? ")
    if(tolower(answer) != "y"){
      stop("Scanning stopped.", call. = FALSE)
    }
  }
}
