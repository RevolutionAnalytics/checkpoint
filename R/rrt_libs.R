#' Write rrt libraries to user's .Rprofile file
#' 
#' @export
#' @keywords internal
#' @param repo Respository path
#' @param repoid Repository id
#' 
#' @details This function is used internally to write a .rrt file in your home directory, and 
#' within that file writes a list of all RRT repositories. 
#' 
#' If for some reason your .rrt file gets deleted, you can also use this function to rewrite that 
#' file and its contents. Ideally you could make a vector or list of all your RRT repositories 
#' and pass those in an apply like functon to this function, like 
#' \code{lapply(repos, rrt_repos_write)}, where \code{repos} is a list of path names to RRT repos.
#' 
#' @examples \dontrun{
#' rrt_repos_write(repo="~/testrepo/")
#' }
rrt_repos_write <- function(repo, repoid=NULL){
  gg <- file.path(Sys.getenv("HOME"), ".rrt")
  append <- if(file.exists(gg)) TRUE else FALSE
  out <- tryCatch(rrt_repos_list(), error=function(e) e)
  if("error" %in% class(out)){
      infofile <- file.path(repo, "rrt", "rrt_manifest.txt")
      info <- readLines(infofile)
      repoidline <- info[grep("RepoID", info)]
      cat(c("\n", sprintf("repo: %s", infofile), repoidline, "__end__"), file = gg, sep = "\n", append = append)
#       cat(c("\n", sprintf("repo: %s", infofile), info, "__end__"), file = gg, sep = "\n", append = append)
  } else {
    existingrepoids <- names(out)
    if(is.null(repoid)){
      linez <- readLines(file.path(repo, "rrt", "rrt_manifest.txt"))
      repoid <- strsplit(linez[grep("RepoID", linez)], ":\\s")[[1]][[2]]
    }
    if(repoid %in% existingrepoids){ NULL } else {
      infofile <- file.path(repo, "rrt", "rrt_manifest.txt")
      info <- readLines(infofile)
      repoidline <- info[grep("RepoID", info)]
      cat(c("\n", sprintf("repo: %s", infofile), repoidline, "__end__"), file = gg, sep = "\n", append = append)
#       cat(c("\n", sprintf("repo: %s", infofile), info, "__end__"), file = gg, sep = "\n", append = append)
    }
  }
}

#' Read rrt libraries from user's .Rprofile file
#' 
#' @export
#' @param repoid Respository id, default is NULL, so gets all repos
#' @examples
#' (repos <- rrt_repos_list())
#' names(repos)
#' rrt_repos_list(names(repos)[1])
#' rrt_repos_list(names(repos)[2])
rrt_repos_list <- function(repoid=NULL){
  gg <- file.path(Sys.getenv("HOME"), ".rrt")
  if(file.exists(gg)){
    hh <- readLines(gg)
    start <- grep("repo:", hh)
    end <- grep("__end__", hh)
    out <- c()
    for(i in seq_along(start)){
      tmp <- hh[start[i] : end[i]]
      tmp <- tmp[-length(tmp)]
      out[[i]] <- do.call(c, lapply(tmp, function(y){ yy <- strsplit(y, ":")[[1]]; zz <- yy[2]; zz <- gsub('\\s+', '', zz); names(zz) <- yy[1]; as.list(zz) }))
    }
    names(out) <- vapply(out, "[[", "", "RepoID")
    
    # get other info from each repo's local manifest file
    out <- lapply(out, function(x){ 
      tmp <- readLines(x$repo)
      tmp2 <- do.call(c, lapply(tmp, function(y){ yy <- strsplit(y, ":")[[1]]; zz <- yy[2]; zz <- gsub('\\s+', '', zz); names(zz) <- yy[1]; as.list(zz) }))
      tmp2$repo_root <- sub("/rrt/rrt_manifest.txt", "", x[['repo']])
      c(x['repo'], tmp2)
    })
    
    class(out) <- 'rrtrepos'
    if(is.null(repoid)) out  else out[[repoid]]
  } else { stop("You have no rrt repos or your .rrt file does not exist.\nIf the latter, run rrt_repos_write() with paths for each RRT repository.") } 
}

#' Print rrtrepos class
#' @method print rrtrepos
#' @export
#' @rdname rrtrepos
print.rrtrepos <- function(x){
  repos <- sapply(x, "[[", "repo")
  repoids <- paste(sapply(x, "[[", "RepoID"), collapse = ", ")
  cat(sprintf("No. repos: %s", length(repos)), "\n")
  cat(sprintf("First 10 repo ids : %s", repoids), "\n")
}

rrt_repos_check <- function(){
  gg <- file.path(Sys.getenv("HOME"), ".rrt")
  if(!file.exists(gg)) rrt_repos_write() else TRUE
  invisible(gg)
}