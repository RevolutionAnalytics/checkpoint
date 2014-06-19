#' Local test for package compatibility
#' 
#' Currently, these checks, tests, etc. are run just on the packages used in your code in the 
#' repository tested - not the dependencies of the packages you use. 
#' 
#' @import testthat devtools digest
#' @export
#' 
#' @details You can see a visual breakdown of check results using \link{rrt_browse} if you have run this 
#' function in your repository.
#' 
#' Details for each option passed to the \code{what}:
#' 
#' \bold{check:} We run \code{devtools::check()}, and skip building the package manual, vignettes, and we
#' don't run examples, or tests. Examples and test are run in separate options passed to the 
#' \code{what} parameter. If check passes for a package TRUE is returned; otherwise FALSE.
#' 
#' \bold{tests:} We first check for the existence of test in the package. If no tests exist, NULL
#' returns. If tests exist we run them with \code{testthat::test_package()}. If tests pass for a 
#' package we return TRUE, otherwise we link to a report for the tests.
#' 
#' \bold{examples:} We run examples. Some examples are wrapped in dontrun, which we don't run by 
#' default, but you can run them by passing on args to \code{devtools::run_examples()}.
#'
#' \bold{update:} We check for any available updates on CRAN for your packages using 
#' \code{old.packages}. NULL is returned if no updates available.  
#' 
#' @param repo Repository path. Defaults to your working directory.
#' @param what What to test, one or more of check, tests, examples, or udpate. \code{match.arg} is
#' used internally so unique abbreviations can be used.
#' @param verbose (logical) Print messages (default) or not
#' 
#' @seealso \link{rrt_browse}
#' 
#' @examples \dontrun{
#' rrt_refresh()
#' rrt_compat(what="update")
#' }

rrt_compat <- function(repo=getwd(), what = 'check', verbose=TRUE)
{
  # Check for appropriate values of what
  what <- match.arg(what, c('check','tests','examples','update'), TRUE)
  
  # set defaults
  checksres <- testsres <- egsres <- oldpkgs <- NULL
  
  ## create repo id using digest
  repoid <- digest(repo)
  
  ## check for repo
  mssg(verbose, "Checking to make sure repository exists...")
  if(!file.exists(repo)){
    mssg(verbose, sprintf("No repository exists at %s", repo))
  }
  
  # check for rrt directory in the repo
  lib <- check_rrt_dir(verbose, repo)
  
  # get pkgs list in the rrt repo
  pkgs <- getpkgslist(repo)
  pkgnames <- getpkgnames(pkgs)
  
  # check: R CMD CHECK via devtools::check
  if("check" %in% what){
    checksres <- lapply(pkgs, checkintmprepo, verbose=verbose)
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

  res <- list(check=checksres, tests=testsres, examples=egsres, update=oldpkgs)
  compatfile <- file.path(repo, "rrt/rrt_check.txt")
  cat(res, file = compatfile)
  
  message("Tests complete!")
}

check_rrt_dir <- function(verbose, repo){
  mssg(verbose, "Checing to make sure rrt directory exists inside your repository...")
  lib <- file.path(repo, "rrt", "lib", R.version$platform, getRversion())
  present <- list.dirs(lib)
  if(!all(grepl("rrt", present))){
    mssg(verbose, "rrt directory doesn't exist...")
  }
  return( lib )
}

getpkgslist <- function(repo){
  pkgs_used <- rrt_deps(repo)
  tocheckpath <- file.path(repo, "rrt", "lib", R.version$platform, getRversion(), "src/contrib")
  pkgs <- list.files(tocheckpath, full.names = TRUE, recursive = FALSE)
  vapply(pkgs_used, function(z){ 
    tmp <- grep(z, pkgs, value = TRUE)
    if(length(tmp) > 1){
      justnames <- sapply(tmp, function(n){ b <- strsplit(n, "/")[[1]]; sub("_.+", "", b[length(b)]) })
      names(justnames)[justnames %in% z]
    } else { tmp }
  }, "", USE.NAMES = FALSE)
}

getpkgnames <- function(zzz){
  sapply(zzz, function(x) 
    strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]], USE.NAMES = FALSE)
}

checkintmprepo <- function(x, verbose){
  pkgname <- strsplit(strsplit(x, "/")[[1]][ length(strsplit(x, "/")[[1]]) ], "_")[[1]][[1]]
  tmpdir <- tempdir()
  untar(x, exdir = tmpdir)
  mssg(verbose, sprintf("Checking %s", pkgname))
  check(file.path(tmpdir, pkgname), document = FALSE, doc_clean = FALSE, cleanup = FALSE, force_suggests = FALSE,
        args = c('--no-manual','--no-vignettes','--no-build-vignettes','--no-examples','--no-tests'))
}