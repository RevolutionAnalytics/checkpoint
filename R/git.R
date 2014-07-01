#' Add and commit files in your RRT repository using git.
#' 
#' @importFrom git2r status repository add init commit
#' @export
#' 
#' @param path Path to the RRT repository
#' @param message Commit message. If you don't pass a message we ask you for a commit message. 
#' Use quotes around your commit message!
#' @param verbose Print messages or not, Default: TRUE
#' @param ... Further args passed on to \code{git2r::commit}
#' 
#' @details Note this doesn't do anything about remote connections to your repo. See help for 
#' \link{remote_add}
#' 
#' @examples \dontrun{
#' git_add_commit(path="~/bbb", message="Made some changes to the regression function")
#' }

git_add_commit <- function(path=getwd(), message="", verbose=TRUE, ...){
  # setup git
  git_setup(path, verbose)
  
  # get repo object
  rep <- git2r::repository(path)
  
  # ask user what files they want to add
  st <- git2r::status(rep)
  untrackedlen <- length(st$untracked)
  if(untrackedlen > 0){
    untrackedtake <- manyselect(sprintf("You have %s untracked files\nWhich do you want to track:\n", untrackedlen))
    switch(untrackedtake, 
           `1` = message("You selected not to add any untracked files - stopping."),
           `2` = message("All untracked files are being added, proceeding..."),
           `3` = message("You selected to add selected untracked files, see ?add in the git2r package - stopping"))
    if(untrackedtake == 2){
      git2r::add(rep, unname(unlist(st$untracked)))
    }
  } else { untrackedtake <- 0 }
  
  if(!untrackedtake == 3){
    # ask user what files they want to add
    st <- git2r::status(rep)
    unstagedlen <- length(st$unstaged)
    if(unstagedlen > 0){
      unstagedtake <- manyselect(sprintf("You have %s unstaged files\nWhich do you want to stage:\n", unstagedlen))
      switch(unstagedtake, 
             `1` = message("You selected not to add any unstaged files - stopping."),
             `2` = message("All unstaged files are being staged, proceeding..."),
             `3` = message("You selected to add selected unstaged files, see ?add in the git2r package - stopping"))
      if(unstagedtake == 2){
        git2r::add(rep, unname(unlist(st$unstaged)))
      }
    } else { unstagedtake <- 0 }
    
    if(!unstagedtake == 3){
      # commit files
      st <- git2r::status(rep)
      stagedlen <- length(st$staged)
      if(stagedlen > 0){
        if(nchar(message) == 0){
          cat("Commit message (quote your message!):")
          message <- scan(n = 50, quiet = TRUE, what = 'character')
        }
        commit(rep, message, ...)
      } else { message("Nothing to commit") }
    }
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

git_setup <- function(path=getwd(), verbose=TRUE, ...){
  # check for rrt directory in the repo, and stop if it doesn't exist
  if(!is_rrt(path, FALSE))
    stop(sprintf("%s is not an RRT repo; See ?rrt_init", path))
  
  # check to see if git initialized yet
  res <- tryCatch(git2r::repository(path), error=function(e) e)
  if("error" %in% class(res)){
#     mssg(verbose, sprintf("%s is not an git repository; Do you want to initialize git?", path))
#     take <- scan(n = 1, quiet = TRUE, what = 'raw')
    take <- yesno(sprintf("%s is not an git repository; Do you want to initialize git?", path))
    if(take == 1){
      mssg(verbose, sprintf("Initializing git repo in %s", path))
      git2r::init(path)
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