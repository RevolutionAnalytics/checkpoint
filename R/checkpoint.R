#' Configures R session to use packages as they existed on CRAN at time of snapshot.
#'
#' Together, the checkpoint package and the checkpoint server act as a CRAN time machine.  The \code{checkpoint()} function installs the packages referenced in the specified project to a local library exactly as they existed at the specified point in time.  Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results.  In this way, anyone using the checkpoint \code{checkpoint()} function can ensure the reproducibility of your scripts or projects at any time.
#'
#' @section Details:
#'
#' \code{checkpoint()} creates a local library into which it installs a copy of the packages required by your project as they existed on CRAN on the specified snapshot date.  Your R session is updated to use only these packages.
#'
#' To automatically determine all packages used in your project, the function scans all R code (\code{.R}, \code{.Rmd}, and \code{.Rpres} files) for \code{\link{library}()} and \code{\link{require}()} statements. In addition, scans for occurrences of code that accesses functions in namespaces using \code{package}\code{\link[base]{::}}\code{foo()} and \code{package}\code{\link[base]{:::}}\code{foo()}. Finally, any occurrences of the functions \code{\link[methods]{setClass}}, \code{\link[methods]{setRefClass}}, \code{\link[methods]{setMethod}} or \code{\link[methods]{setGeneric}} will also identify the \code{methods} package as a dependency.
#'
#' Specifically, the function will:
#'
#' \itemize{
#' \item{Create a new local snapshot library to install packages.  By default this library folder is at \code{~/.checkpoint}} but you can modify the path using the \code{checkpointLocation} argument.
#' \item{Update the options for your CRAN mirror and point to an MRAN snapshot using \code{\link[base]{options}(repos)}}
#' \item{Scan your project folder for all required packages and install them from the snapshot using \code{\link[utils]{install.packages}}}
#' }
#'
#' @section Resetting the checkpoint:
#' To reset the checkpoint, simply restart your R session.
#'
#' @param snapshotDate Date of snapshot to use in \code{YYYY-MM-DD} format,e.g. \code{"2014-09-17"}.  Specify a date on or after \code{"2014-09-17"}.  MRAN takes one snapshot per day.
#'
#' @param project A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for \code{snapshotDate}.  Defaults to current working directory using \code{\link{getwd}()}.
#' 
#' @param R.version Optional character string, e.g. "3.1.2".  If specified, compares the current \code{\link[base]{R.version}} to the specified R.version. If these differ, stops processing with an error, making no changes to the system. Specifically, if the check fails, the library path is NOT modified. This argument allows the original script author to specify a specific version of R to obtain the desired results.
#'
#' @param scanForPackages If TRUE, scans for packages in project folder (see details). If FALSE, skips the scanning process.  A use case for \code{scanForPackages = FALSE} is to skip the scanning and installation process, e.g. in production environments with a large number of R scripts in the project.  Only set \code{scanForPackages = FALSE} if you are certain that all package dependencies are already in the checkpoint folder.
#' 
#' @param checkpointLocation File path where the checkpoint library is stored.  Default is \code{"~/"}, i.e. the user's home directory. A use case for changing this is to create a checkpoint library on a portable drive (e.g. USB drive).
#' 
#' @param use.knitr If TRUE, uses parses all \code{Rmarkdown} files using the \code{knitr} package.  
#'
#' @param verbose If TRUE, displays progress messages.
#'
#'
#' @return NULL.  See the \code{Details} section for side effects.
#'
#' @export
#'
#' @example /inst/examples/example_checkpoint.R
#'
#' @importFrom utils install.packages

checkpoint <- function(snapshotDate, project = getwd(), R.version, scanForPackages = TRUE,
                       checkpointLocation = "~/",
                       verbose=TRUE, 
                       use.knitr = system.file(package="knitr") != "") {

  if(!missing("R.version") && !is.null(R.version)){
    if(!correctR(as.character(R.version))){
      message <- sprintf("Specified R.version %s does not match current R (%s)", 
                         R.version, utils::packageVersion("base"))
      mssg(verbose, message)
      mssg(verbose, "Terminating checkpoint")
      mssg(verbose, "---")
      stop(message)
    }
  }
  
  fixRstudioBug()
  
  if(!createFolders(snapshotDate = snapshotDate, checkpointLocation = checkpointLocation))
    stop("Unable to create checkpoint folders at checkpointLocation = \"", checkpointLocation, "\"")
  
  
  snapshoturl <- getSnapshotUrl(snapshotDate=snapshotDate)


  compiler.path <- system.file(package = "compiler", lib.loc = .Library[1])
  # set repos
  setMranMirror(snapshotUrl = snapshoturl)

  libPath <- checkpointPath(snapshotDate, type = "lib", checkpointLocation = checkpointLocation)
  installMissingBasePackages()
  
  # Set lib path
  setLibPaths(libPath = libPath)
    
  # Scan for packages used
  exclude.packages = c("checkpoint", # this very package
                       c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
                         "methods", "parallel", "splines", "stats", "stats4", "tcltk",
                         "tools", "utils"))  # all base priority packages, not on CRAN or MRAN
  packages.installed <- unname(installed.packages()[, "Package"])
  
  if(isTRUE(scanForPackages)){
    mssg(verbose, "Scanning for packages used in this project")
    pkgs <- projectScanPackages(project, use.knitr = use.knitr)
    packages.detected <- pkgs[["pkgs"]]
    mssg(verbose, "- Discovered ", length(packages.detected), " packages")
    
    if(length(pkgs[["error"]]) > 0){
      mssg(verbose, "Unable to parse ", length(pkgs[["error"]]), " files:")
      for(file in pkgs[["error"]])  mssg(verbose, "- ", file)
    }
  } else {
    packages.detected <- character(0)
  }
  
  
  packages.to.install <- setdiff(packages.detected, c(packages.installed, exclude.packages))

  # detach checkpointed pkgs already loaded
  
  packages.in.search <- findInSearchPath(packages.to.install)
  detachFromSearchPath(packages.in.search)
  
  # check if packages are available in snapshot
  
  if(length(packages.to.install) > 0) {
    not.available <- !packages.to.install %in% available.packages()[, "Package"]
    if(sum(not.available > 0)){
      mssg(verbose, "Packages not available in repository and won't be installed:")
      for(pkg in packages.to.install[not.available]) mssg(verbose, " - ", pkg)
      packages.to.install <- packages.to.install[!not.available]
    }
  }
  
  # install missing packages

  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this project ")
    for(pkg in packages.to.install){
      if(pkg %in% unname(installed.packages()[, "Package"])) {
        mssg(verbose, " - Previously installed ", sQuote(pkg))
      } else {
        mssg(verbose, " - Installing ", sQuote(pkg))
        suppressWarnings(
          utils::install.packages(pkgs = pkg, verbose = FALSE, quiet = TRUE)
        )
      }
    }
  } else if(length(packages.detected > 0)){
    mssg(verbose, "All detected packages already installed")
  } else {
    if(isTRUE(scanForPackages)) mssg(verbose, "No packages found to install")
  }
  
  # Reload detached packages
  if(length(packages.in.search > 0)){
    lapply(packages.in.search, library, character.only = TRUE, quietly = TRUE)
  }
  
  mssg(verbose, "checkpoint process complete")
  mssg(verbose, "---")
  invisible(NULL)}

setMranMirror <- function(snapshotDate, snapshotUrl = checkpoint:::getSnapShotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(snapshotDate, libPath=checkpointPath(snapshotDate, type = "lib")){
    assign(".lib.loc", c(libPath, checkpointBasePkgs()), envir = environment(.libPaths))}

mranUrl <- function()"http://mran.revolutionanalytics.com/snapshot/"

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
  on.exit(close(con), add = TRUE)
  tryCatch(
    suppressWarnings(readLines(con)),
    error =
      function(e) {
        stop("Unable to find snapshot on MRAN")})
  snapshot.url}


mssg <- function(x, ...) if(x) message(...)

correctR <- function(x) compareVersion(as.character(utils::packageVersion("base")), x) == 0

