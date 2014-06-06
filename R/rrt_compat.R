#' Local test for package compatibility
#' 
#' @import testthat
#' @export
#' @param repo Repository path
#' @param what What to test, one or more of install, tests, examples.
#' @examples
#' rrt_compat(repo="~/testrepo")
rrt_compat <- function(repo)
{
  # setup
  ## create repo id using digest
  repoid <- digest(repo)
  if(is.null(repo)) repo <- getwd()
  
  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }
  
  # check for rrt directory in the repo, and create if doesn't exist already
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(repo)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }
  
  # test installation
  # Note: maybe not do anything, other than check to make sure packages were installed and load
  
  # run tests
#   test_package(pkg)
  
  # run examples
#   run_examples(pkg)
  
  # any other tests to run?
  message("excuse my dust, still working on this...")
}