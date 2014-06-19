#' Local test for package compatibility
#' 
#' Currently, these checks, tests, etc. are run just on the packages used in your code in the 
#' repository tested - not the dependencies of the packages you use. 
#' 
#' @import testthat devtools digest
#' @export
#' @param repo Repository path
#' @param what What to test, one or more of check, tests, examples, or udpate. \code{match.arg} is
#' used internally so unique abbreviations can be used.
#' @param verbose (logical) Print messages (default) or not
#' @examples \dontrun{
#' rrt_compat(repo="~/testrepo")
#' }

rrt_compat <- function(repo=getwd(), what = 'check', verbose=TRUE)
{
  # Check for appropriate values of what
  what <- match.arg(what, c('check','tests','examples','update'), TRUE)
  
  # setup
  ## create repo id using digest
  repoid <- digest(repo)
  
  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }
  
  # check for rrt directory in the repo
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(lib)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }
  
  # get pkgs list in the rrt repo
  pkgs_used <- rrt_deps(repo)
  tocheckpath <- file.path(repo, "rrt", "lib", R.version$platform, getRversion(), "src/contrib")
  pkgs <- list.files(tocheckpath, full.names = TRUE, recursive = FALSE)
  pkgs <- vapply(pkgs_used, function(z) grep(z, pkgs, value = TRUE), "", USE.NAMES = FALSE)
  pkgnames <- sapply(pkgs, function(x) strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]], USE.NAMES = FALSE)
  
  # test installation
  # Note: maybe not do anything, other than check to make sure packages were installed and load
  
  # R CMD CHECK
  if("check" %in% what){
    checkintmprepo <- function(x){
      pkgname <- strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]]
      tmpdir <- tempdir()
      untar(x, exdir = tmpdir)
      mssg(verbose, sprintf("Checking %s", pkgname))
      check(file.path(tmpdir, pkgname), document = FALSE, doc_clean = FALSE, cleanup = FALSE, force_suggests = FALSE,
            args = c('--no-manual','--no-vignettes','--no-build-vignettes','--no-examples','--no-tests'))
    }
#     checkintmprepo(pkgs[[4]])
    checksres <- lapply(pkgs, checkintmprepo)
    names(checksres) <- pkgnames
  }
  
  # run tests
  if("tests" %in% what){
#     testpaths <- file.path(lib, pkgnames)
#     lapply(pkgnames, test_package)
    testsres <- "not done yet"
  }
  
  # run examples
  if("examples" %in% what){
#     lapply(pkgs, run_examples)
    egsres <- "not done yet"
  }

  # check for packages that need updating
  if("update" %in% what){
    oldpkgs <- old.packages(lib)
    oldpkgs <- oldpkgs[,c('Package','Installed','ReposVer')]
  }

  invisible(list(check=checkres, tests=testsres, examples=egsres, update=oldpkgs))
  message("Tests complete!")
}