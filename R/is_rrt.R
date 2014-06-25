#' Find out if a repository is an RRT repository.
#'
#' @export
#' @param repo (character) A path to create a RRT repository; defaults to current working directory
#' @param verbose Print messages or not, Default: TRUE
#' @return A logical, TRUE or FALSE
#'
#' @examples \dontrun{
#' is_rrt(repo="~/testrepo")
#' }

is_rrt <- function(repo=getwd(), verbose=TRUE)
{
  if(!file.exists(file.path(repo, "rrt"))){
    mssg(verbose, "Nope, this isn't an rrt repository\n"); FALSE
  } else { mssg(verbose, "Cool - this is an RRT repository :)\n"); TRUE }
}