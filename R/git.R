#' Add and commit files in your RRT repository
#' 
#' @import git2r
#' @export
#' 
#' @param path Path to the RRT repository
#' @param verbose Print messages or not, Default: TRUE
#' 
#' @examples \dontrun{
#' git_add(path="~/bbb")
#' }

git_add <- function(path=getwd(), verbose=TRUE){
  # setup git
  git_setup(path, verbose)
  
  # get repo object
  rep <- repository(path)
  
  # ask user what files they want to add
  st <- status(rep)
  untrlen <- length(st$untracked)
  if(untrlen > 0){
#     message(sprintf("You have %s untracked files\nWhich do you want to track:", untrlen))
    take <- manyselect(sprintf("You have %s untracked files\nWhich do you want to track:\n", untrlen))
    switch(take, 
           `1` = message("You selected not to add any untracked files, proceeding..."),
           `2` = message("All untracked files are being added, proceeding..."),
           `3` = message("You selected to add selected untracked files, see ?add in the git2r package."))
  }
  
  # add untracked files users chooses
  if(take == 2){
    add(rep, unname(unlist(st$untracked)))
  }

  # commit files users chooses
  if(take == 2){
    commit(rep, unname(unlist(st$untracked)))
  }
}

#' Add and commit files in your RRT repository
#' 
#' @export
#' 
#' @param path Path to the RRT repository
#' @param verbose Print messages or not, Default: TRUE
#' @param ... Further args, ignored for now.
#' 
#' @examples \dontrun{
#' git_add(path="~/bbb")
#' }

git_setup <- function(path, verbose=TRUE, ...){
  # check for rrt directory in the repo, and stop if it doesn't exist
  if(!is_rrt(path, FALSE))
    stop(sprintf("%s is not an RRT repo; See ?rrt_init", path))
  
  # check to see if git initialized yet
  res <- tryCatch(repository(path), error=function(e) e)
  if("error" %in% class(res)){
#     mssg(verbose, sprintf("%s is not an git repository; Do you want to initialize git?", path))
#     take <- scan(n = 1, quiet = TRUE, what = 'raw')
    take <- yesno(sprintf("%s is not an git repository; Do you want to initialize git?", path))
    if(!take == 1){
      mssg(verbose, sprintf("Initializing git repo in %s", path))
      init(path)
      mssg(verbose, sprintf("Writing .gitignore file in %s", path))
      add_gitignore(path)
    } else { mssg(verbose, "Okay, doing nothing") }
  }
}

# git_clone <- function(url, local_path=getwd()){
#   clone(url, local_path)
# }

add_gitignore <- function(path){
  gignore <- file.path(path, ".gitignore")
  ignore <- c('rrt','!rrt/rrt_manifest.yml','.Rprofile','.DS_Store')
  cat(ignore, file = gignore, sep = "\n")
}

yesno <- function(question){
  cat(question)
  menu(c('yes','no'))
}

manyselect <- function(question){
  cat(question)
  menu(c('none', 'all', 'pick'))
}