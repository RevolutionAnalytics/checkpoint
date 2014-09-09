
#' Functions to check if an object is a rrt_repo, or coerce it if possible.
#' 
#' @param x A list to coerce into an rrt_repo object
#' @export
#' @rdname as.rrt_repo
is.rrt_repo <- function(x){
  inherits(x, "rrt_repo")
}


#' @export
#' @rdname as.rrt_repo
as.rrt_repo <- function(x){
  if(is.null(x)) x <- list()
  if(is.rrt_repo(x)){
    return(x)
  } else {
    class(x) <- "rrt_repo"
    x
  }
}



# repo <- function(x)x$repo
# reponame <- function(x)x$reponame


rrtInteractive <- function(){
  message("\nRepository name (default: random name generated):")
  randomname <- paste0(sample(letters, 10), collapse = "")
  x$reponame <- rrt_readline(randomname)
  
  message("\nRepository path (default: home directory + repository name):")
  defaultpath <- file.path(Sys.getenv("HOME"), x$reponame)
  x$repo <- rrt_readline(defaultpath)
  
  message("\nRepository author(s) (default: left blank):")
  x$author <- rrt_readline()
  message("\nRepo license (default: MIT):")
  x$license <- rrt_readline("MIT")
  message("\nRepo description (default: left blank):")
  x$description <- rrt_readline()
  message("\nRepo remote git or svn repo (default: left blank):")
  x$remote <- rrt_readline()
  
  
  class(x) <- "rrt"
  x
}
