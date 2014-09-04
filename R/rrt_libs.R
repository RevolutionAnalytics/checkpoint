#' Write rrt libraries to user's global list of RRT repos
#'
#' @param repo Respository path
#' @param repoid Repository id
#'
#' @rdname rrt_repos_list
#' @export
#' @family rrt
#'
#' @examples \dontrun{
#' rrt_repos_write(repo="~/testrepo/")
#' }
rrt_repos_write <- function(repo, repoid=NULL){
  rrtdir <- rrtPath(repo, "rootdir")
  if(!file.exists(rrtdir)) dir.create(rrtdir, recursive = TRUE)
  
  rrtTextFile <- file.path(rrtdir, "rrt.txt")
  append <- if(file.exists(rrtTextFile)) TRUE else FALSE
  out <- tryCatch(rrt_repos_list(), error=function(e) e)
  if("error" %in% class(out)){
      infofile <- rrtPath(repo, "manifest")
      info <- readLines(infofile)
      repoidline <- info[grep("RepoID", info)]
      cat(c("\n", sprintf("repo: %s", infofile), repoidline, "__end__"), 
          file = rrtTextFile, sep = "\n", append = append)
  } else {
    existingRepoIds <- names(out)
    if(is.null(repoid)){
      linez <- readLines(rrtPath(repo, "manifest"))
      repoid <- strsplit(linez[grep("RepoID", linez)], ":\\s")[[1]][[2]]
    }
    if(repoid %in% existingRepoIds){ NULL } else {
      infofile <- path.expand(rrtPath(repo, "manifest"))
      info <- readLines(infofile)
      repoidline <- info[grep("RepoID", info)]
      cat(c("\n", sprintf("repo: %s", infofile), repoidline, "__end__"), 
          file = rrtTextFile, sep = "\n", append = append)
    }
  }
}



#' Remove any missing RRT repos from the user's global list of RRT repos
#'
#' @param repo (character) A path to create a RRT repository; defaults to current working directory
#' @param verbose Print messages or not, Default: TRUE
#' 
#' @return Prints message saying what has been done.
#' @export
#' @family rrt
#'
rrt_repos_remove <- function(repo=getwd(), verbose=TRUE)
{
  if(is_rrt(repo, verbose)){
    rrtrepos <- rrt_repos_list()
    repos <- sapply(rrtrepos, "[[", "repo")
    repoids <- paste(sapply(rrtrepos, "[[", "RepoID"), collapse = ", ")
    paths <- lapply(rrtrepos, function(b) c(b$repo_root, b$RepoID))
    misspaths <- paths[vapply(paths, function(n) is.na(n[[1]]), TRUE)]
    notmisspaths <- paths[!vapply(paths, function(n) is.na(n[[1]]), TRUE)]
    
    if(!length(misspaths) == 0){
      idmiss <- names(misspaths)
      idmiss_list <- rrtrepos[names(rrtrepos) %in% idmiss]
      idmisstodf <- list()
      for(i in seq_along(idmiss_list)){
        tmp <- idmiss_list[[i]]$repo
        tmp <- sub("/rrt/rrt_manifest.yml", "", tmp)
        idmisstodf[[i]] <- c(tmp, names(idmiss_list[i]))
      }
      dfmiss <- data.frame(do.call(rbind, idmisstodf), stringsAsFactors = FALSE)
      names(dfmiss) <- c('path','repoid')
      # remove repos - wipe current rrt.txt file first
      gg <- rrtPath(repo, "rootfile")
      file.remove(gg)
      invisible(sapply(vapply(notmisspaths, 
                              function(x) x[[1]], character(1)), rrt_repos_write))
      mssg(verbose, "Removed the following from the global list of RRT repos\n:")
      print(dfmiss)
    } else { mssg(verbose, "No missing RRT repos to remove.") }
  }
}



#' Read rrt libraries from user's global list of RRT repos.
#'
#' This function is used internally to write a .rrt file in your home directory, and within that file writes a list of all RRT repositories.
#'
#' If for some reason your .rrt file gets deleted, you can also use this function to rewrite that file and its contents. Ideally you could make a vector or list of all your RRT repositories and pass those in an \code{apply} like function to this function, e.g:
#' 
#'  \code{lapply(repos, rrt_repos_write)}
#'  
#'  where \code{repos} is a list of path names to RRT repos.
#'
#' @export
#' @family rrt
#' @examples \dontrun{
#' repos <- rrt_repos_list()
#' names(repos)
#' }
rrt_repos_list <- function(repoid=NULL){
  gg <- rrtPath(type="rootfile")
  if(file.exists(gg)){
    hh <- readLines(gg)
    start <- grep("repo:", hh)
    end <- grep("__end__", hh)
    out <- c()
    for(i in seq_along(start)){
      tmp <- hh[start[i] : end[i]]
      tmp <- tmp[-length(tmp)]
      out[[i]] <- do.call(c, lapply(tmp, function(y){ 
        yy <- strsplit(y, ": ")[[1]]
        zz <- yy[2]
        if(.Platform$OS.type != "windows") zz <- gsub('\\s+', '', zz)
        names(zz) <- yy[1]; as.list(zz) 
      }))
    }
    names(out) <- vapply(out, "[[", "", "RepoID")

    # get other info from each repo's local manifest file
    out <- lapply(out, function(x){
      if(file.exists(x$repo)){
        tmp2 <- yaml.load_file(x$repo)
        tmp2$repo_root <- sub("/rrt/rrt_manifest.yml", "", x[['repo']])
        c(x['repo'], tmp2, missing=FALSE)
      } else {
        addfields <- c(RepositoryName = NA, Authors = NA, License = NA, 
                       Description = NA, Remote = NA, InstalledWith = NA, 
                       InstalledFrom = NA, RRT_version = NA, 
                       R_version = NA, DateCreated = NA, PkgsInstalledAt = NA, 
                       x['RepoID'], 
                       Packages = NA, SystemRequirements = NA, repo_root = NA)
        c(x['repo'], addfields, missing=TRUE)
      }
    })

    class(out) <- 'rrtrepos'
    if(is.null(repoid)) out else out[[repoid]]
  } else { 
    msg <- paste("You have no rrt repos or your .rrt file does not exist.", 
                 "If the latter, run rrt_repos_write() with paths for each RRT repository.",
                 sep="\n")
    stop(msg) 
  }
}




#' Print rrtrepos class
#' @method print rrtrepos
#' @export
#' @param x Input
#' @param ... not used
#' @rdname rrt_repos_list

print.rrtrepos <- function(x, ...){
  repos <- sapply(x, "[[", "repo")
  repoids <- paste(sapply(x, "[[", "RepoID"), collapse = ", ")
  paths <- lapply(x, function(b) c(b$repo_root, b$RepoID))
  misspaths <- paths[vapply(paths, function(n) is.na(n[[1]]), TRUE)]
  notmisspaths <- paths[!vapply(paths, function(n) is.na(n[[1]]), TRUE)]

  df <- data.frame(do.call(rbind, unname(notmisspaths)), stringsAsFactors = FALSE)
  names(df) <- c('path','repoid')

  if(!length(misspaths) == 0){
    idmiss <- names(misspaths)
    idmiss_list <- x[names(x) %in% idmiss]
    idmisstodf <- list()
    for(i in seq_along(idmiss_list)){
      tmp <- idmiss_list[[i]]$repo
      tmp <- sub("/rrt/rrt_manifest.yml", "", tmp)
      idmisstodf[[i]] <- c(tmp, names(idmiss_list[i]))
    }
    dfmiss <- data.frame(do.call(rbind, idmisstodf), stringsAsFactors = FALSE)
    names(dfmiss) <- c('path','repoid')
  } else { dfmiss <- "all good :)" }

  cat(wrap(sprintf("Number of RRT repos: %s", length(repos)), width = 80), "\n\n")
  cat(wrap("RRT repos : \n\n", width = 80), "\n\n")
  print(df)
  cat("\n\n")
  cat(wrap("RRT repos in .rrt file, but repo not found: \n\n", width = 80), "\n\n")
  print(dfmiss)
  cat("\n\n")
  cat("Heads up -> To get details for a single RRT repo: rrt_repos_list('<repoid>')\n\n")
}

wrap <- function (..., indent = 0, width=getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 2, width = width)
  paste0(wrapped, collapse = "\n")
}

rrt_repos_check <- function(){
  gg <- rrtPath(type="rootfile")
  if(!file.exists(gg)) rrt_repos_write() else TRUE
  invisible(gg)
}
