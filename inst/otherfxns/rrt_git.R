#' Commit versions of an RRT repository to git
#'
#' @import digest
#' @template rrt
#' @keywords internal
#'
#' @return Files written to the user system, with informative messages on progress
#'
#' @examples \dontrun{
#' rrt_git(repo="~/testrepo")
#' }

rrt_git <- function(repo=NULL, verbose=TRUE)
{

  # create repo id using digest
  repoid <- digest(repo)
  if(is.null(repo)) repo <- getwd()

  # initiate git repo
  mssg(verbose, sprintf("git initiated in %s ...", repo))
  repo2 <- init(repo)

  # Add a gitignore file
  mssg(verbose, "Adding hidden file .gitignore ...")
  gitignore <- c(".DS_Store",".Rhistory")
  cat(gitignore, file = file.path(repo, ".gitignore"), sep = "\n")

  # stage files to be tracked
  mssg(verbose, "Adding all files to be tracked ...")
  allfiles <- list.files(repo, full.names = FALSE, include.dirs = TRUE, all.files = TRUE)
  allfiles <- allfiles[!allfiles %in% c('.','..','.DS_Store','.git')]
  backto <- getwd()
  setwd(repo)
  for(i in seq_along(allfiles)) add(repo2, allfiles[[i]])
  setwd(backto)

  # Commit files
  commit(repo2, message = "changes made!")
}
