#' Configures R session to use packages as they existed on CRAN at a snapshot date.
#'
#' Together, the RRT package and the MRAN server act as a CRAN time machine. The \code{checkpoint()} function downloads the packages to a local library exactly as they were at the specified point in time. Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results. In this way, anyone using RRT's \code{checkpoint()} can ensure the reproducibility of your scripts or projects at any time.
#' 
#' @section Details:
#' 
#' \code{checkpoint()} creates a local library into which it installs a copy of the packages required by your project as they were on CRAN on the specified snapshot date. Your R session is updated to use only these packages.
#'
#' To automatically determine all packages used in your project, the function scans all R code files for \code{library()} and \code{requires()} statements.  During this scan, the function searches for all R and Rmarkdown scripts, in other words file with extensions \code{.R}, \code{.Rmd} and \code{.Rpres}.
#' 
#' Specifically, the function will:
#' 
#' \itemize{
#' \item{Create a new local snapshot library to download packages. This library folder is at \code{~/.rrt}}
#' \item{Update the options for your CRAN mirror and point to an MRAN snapshot using \code{options(repos)}}
#' \item{Scan your project folder for all required packages and install them from the snapshot using \code{\link[utils]{download.packages}}}
#' }
#'
#'
#' @param snapshotDate Date of snapshot to use in \code{YYYY-MM-DD} format,e.g. \code{"2014-09-17"}.  Daily snapshots exists on the MRAN server starting from \code{"2014-09-17"}.
#'
#' @param project A project path. This is the path to the root of the project you want checkpointed. Defaults to current working directory using \code{\link{getwd}()}.
#'
#'
#' @param verbose If TRUE, displays progress messages.
#'
#'
#' @return NULL. See the \code{details} section for side effects.
#'
#' @export
#'
#' @example /inst/examples/example_checkpoint.R
#'

checkpoint <- function(snapshotDate, project = getwd(), verbose=TRUE) {

  createFolders(snapshotDate)
  snapshoturl <- getSnapshotUrl(snapshotDate=snapshotDate)

  # set repos
  setMranMirror(snapshotUrl = snapshoturl)

  # Set lib path
  setLibPaths(snapshotDate)

  mssg(verbose, "Scanning for loaded pkgs")

  # Scan for packages used
  mssg(verbose, "Scanning for packages used in this project")
  packages.to.install = projectScanPackages(project)

  # download and install missing packages

  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this project ")
    utils::install.packages(pkgs = packages.to.install, verbose=FALSE, quiet=TRUE)
  } else {
    mssg(verbose, "No packages found to install")
  }

  # detach and reload checkpointed pkgs already loaded
  search.path = search()
  lapply(
    unlist(
      lapply(
        packages.to.install,
        grep,
        x = search.path)),
    function(x) {
      detach(x, unload = TRUE, force = TRUE)
      library(search.path[x], character.only = TRUE)})

  NULL}

setMranMirror <- function(snapshotDate, snapshotUrl = RRT:::getSnapShotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(snapshotDate, libPath=rrtPath(snapshotDate, "lib")){
  assign(".lib.loc", libPath, envir = environment(.libPaths))}

mranUrl <- function()"http://cran-snapshots.revolutionanalytics.com/"

getSnapshotUrl <- function(snapshotDate, url = mranUrl()){
  mran.root = url(url)
  on.exit(close(mran.root))
  tryCatch(
    suppressWarnings(readLines(mran.root)),
    error =
      function(e) {
        stop(sprintf("Unable to reach MRAN: %s", e$message))})
  snapshot.url = paste(gsub("/$", "", url), snapshotDate, sep = "/")
  con = url(snapshot.url)
  on.exit(close(con))
  tryCatch(
    suppressWarnings(readLines(con)),
    error =
      function(e) {
        stop("Unable to find snapshot on MRAN")})
  snapshot.url}


mssg <- function(x, ...) if(x) message(...)
