#' Configures R session to use packages as they existed on CRAN at time of snapshot.
#'
#' Together, the checkpoint package and the checkpoint server act as a CRAN time machine.  The `checkpoint()` function installs the packages referenced in the specified project to a local library exactly as they existed at the specified point in time.  Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results.  In this way, anyone using the checkpoint `checkpoint()` function can ensure the reproducibility of your scripts or projects at any time.
#'
#' @section Details:
#'
#' `checkpoint()` creates a local library into which it installs a copy of the packages required by your project as they existed on CRAN on the specified snapshot date.  Your R session is updated to use only these packages.
#'
#' To automatically determine all packages used in your project, the function scans all R code (`.R`, `.Rmd`, and `.Rpres` files) for [library()] and [require()] statements. In addition, scans for occurrences of code that accesses functions in namespaces using `package[::]foo()` and `package[:::]foo()`. Finally, any occurrences of the functions [methods::setClass], [methods::setRefClass], [methods::setMethod] or [methods::setGeneric] will also identify the `methods` package as a dependency.
#'
#' Specifically, the function will:
#'
#' * Create a new local snapshot library to install packages.  By default this library folder is at `~/.checkpoint` but you can modify the path using the `checkpointLocation` argument.
#' * Update the options for your CRAN mirror and point to an MRAN snapshot using [options]`(repos)`
#' * Scan your project folder for all required packages and install them from the snapshot using [utils::install.packages()]
#'
#' @section Resetting the checkpoint:
#' To reset the checkpoint, simply restart your R session.
#' 
#' @section Changing the default MRAN url:
#' 
#' `checkpoint` uses https by default to download packages (see \url{https://www.r-consortium.org/blog/2015/08/17/best-practices-for-using-r-securely}).
#' `checkpoint` Defaults to \url{https://mran.microsoft.com/snapshot} by default in R versions 3.2.0 and later, if https support is enabled.
#' 
#' You can modify the default URL. To change the URL, use `options(checkpoint.mranUrl = ...)`
#' 
#' @section Log file:
#' 
#' As a side effect, the `checkpoint` function writes a log file with information about the downloaded files, in particular the package downloaded and the associated file size in bytes. The log is stored at the root of the `checkpointLocation`. For example, if `checkpointLocation` is the user home folder (the default) then the log file is at `~/.checkpoint/checkpoint_log.csv`. This file contains columns for:
#' 
#' * `timestamp`
#' * `snapshotDate`
#' * `pkg`
#' * `bytes`
#'
#' @param snapshotDate Date of snapshot to use in `YYYY-MM-DD` format,e.g. `"2014-09-17"`.  Specify a date on or after `"2014-09-17"`.  MRAN takes one snapshot per day.
#'
#' @param project A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for `snapshotDate`.  Defaults to current working directory using [getwd()].
#'
#' @param R.version Optional character string, e.g. "3.1.2".  If specified, compares the current [R.version] to the specified R.version. If these differ, stops processing with an error, making no changes to the system. Specifically, if the check fails, the library path is NOT modified. This argument allows the original script author to specify a specific version of R to obtain the desired results.
#'
#' @param scanForPackages If `TRUE`, scans for packages in project folder (see details). If FALSE, skips the scanning process.  A use case for `scanForPackages = FALSE` is to skip the scanning and installation process, e.g. in production environments with a large number of R scripts in the project.  Only set `scanForPackages = FALSE` if you are certain that all package dependencies are already in the checkpoint folder.
#'
#' @param checkpointLocation File path where the checkpoint library is stored.  Default is `"~/"`, i.e. the user's home directory. A use case for changing this is to create a checkpoint library on a portable drive (e.g. USB drive).
#'
#' @param use.knitr If `TRUE`,  parses all `Rmarkdown` files using the `knitr` package.
#' 
#' @param auto.install.knitr If `TRUE` and the project contains rmarkdown files, then automatically included the packages `knitr` and `rmarkdown` in packages to install.
#' 
#' @param scan.rnw.with.knitr If `TRUE`, uses [knitr::knit()] to parse `.Rnw` files, otherwise use [utils::Sweave()]
#'
#' @param verbose If `TRUE`, displays progress messages.
#' 
#' @param forceInstall If `TRUE`, forces the re-installation of all discovered packages and their dependencies. This is useful if, for some reason, the checkpoint archive becomes corrupted.
#'
#' @param forceProject If `TRUE`, forces the checkpoint process, even if the provided project folder doesn't look like an R project. A commonly reported user problem is that they accidentally trigger the checkpoint process from their home folder, resulting in scanning many R files and downloading many packages. To prevent this, we use a heuristic to determine if the project folder looks like an R project. If the project folder is the home folder, and also contains no R files, then `checkpoint()` asks for confirmation to continue.

#'
#' @return Checkpoint is called for its side-effects (see the details section), but invisibly returns a list with elements:
#' * `files_not_scanned`
#' * `pkgs_found`
#' * `pkgs_not_on_mran`
#' * `pkgs_installed`
#' 
#'
#' @export
#' 
#' @family checkpoint functions
#'
#' @example /inst/examples/example_checkpoint.R
#'
#' @importFrom utils install.packages

checkpoint <- function(snapshotDate, project = getwd(),
                       R.version, 
                       scanForPackages = TRUE,
                       checkpointLocation = "~/",
                       verbose=TRUE,
                       use.knitr = system.file(package="knitr") != "", 
                       auto.install.knitr = TRUE,
                       scan.rnw.with.knitr = FALSE,
                       forceInstall = FALSE, 
                       forceProject = FALSE) {
  
  if(interactive()) validateProjectFolder(project)
  
  stopIfInvalidDate(snapshotDate)
  
  if(!missing("R.version") && !is.null(R.version)){
    if(!correctR(as.character(R.version))){
      message <- sprintf(
        "Specified R.version %s does not match current R (%s)",
        R.version, utils::packageVersion("base"))
      mssg(verbose, message)
      mssg(verbose, "Terminating checkpoint")
      mssg(verbose, "---")
      stop(message)
    }
  }
  
  checkpointLocation = authorizeFileSystemUse(checkpointLocation)
  
  fixRstudioBug()
  
  if(!createFolders(snapshotDate = snapshotDate, 
                    checkpointLocation = checkpointLocation))
    stop("Unable to create checkpoint folders at checkpointLocation = \"",
         checkpointLocation, "\"")
  
  
  mran <- mranUrl()
  snapshoturl <- getSnapshotUrl(snapshotDate = snapshotDate)
  
  
  compiler.path <- system.file(package = "compiler", lib.loc = .Library[1])
  
  libPath <- checkpointPath(snapshotDate, type = "lib", 
                            checkpointLocation = checkpointLocation)
  installMissingBasePackages(checkpointLocation = checkpointLocation)
  
  # Set lib path
  setLibPaths(checkpointLocation = checkpointLocation, libPath = libPath)
  
  # Scan for packages used
  exclude.packages = c(
    # this very package
    "checkpoint", 
    # all base priority packages, not on CRAN or MRAN
    c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
      "methods", "parallel", "splines", "stats", "stats4", "tcltk",
      "tools", "utils")
  )  
  packages.installed <- unname(installed.packages()[, "Package"])
  
  if(isTRUE(scanForPackages)){
    mssg(verbose, "Scanning for packages used in this project")
    pkgs <- projectScanPackages(project, use.knitr = use.knitr, 
                                scan.rnw.with.knitr = scan.rnw.with.knitr)
    packages.detected <- pkgs[["pkgs"]]
    mssg(verbose, "- Discovered ", length(packages.detected), " packages")
    
    if(length(pkgs[["error"]]) > 0){
      files.not.parsed <- pkgs[["error"]]
      mssg(verbose, "Unable to parse ", length(pkgs[["error"]]), " files:")
      for(file in files.not.parsed)  mssg(verbose, "- ", file)
    } else {
      files.not.parsed <- character(0)
    }
  } else {
    packages.detected <- character(0)
    files.not.parsed <- character(0)
  }
  
  if(forceInstall && packages.detected > 0){
    to_remove <- as.vector(unlist(
      tools::package_dependencies(packages.detected)
    ))
    to_remove <- c(packages.detected, to_remove)
    tryCatch(
      suppressMessages(suppressWarnings(
        utils::remove.packages(to_remove)
      )),
      error = function(e)e
    )
    packages.to.install <- packages.detected
    packages.installed <- character(0)
  } else {
    packages.to.install <- setdiff(packages.detected, c(packages.installed, exclude.packages))
  }
  
  # detach checkpointed pkgs already loaded
  
  packages.in.search <- findInSearchPath(packages.to.install)
  detachFromSearchPath(packages.in.search)
  
  # check if packages are available in snapshot
  
  if(length(packages.to.install) > 0) {
    # set repos
    setMranMirror(snapshotUrl = snapshoturl)
    not.available <- !packages.to.install %in% available.packages()[, "Package"]
    if(sum(not.available > 0)){
      mssg(verbose, 
           "Packages not available in repository and won't be installed:")
      for(pkg in packages.to.install[not.available]) mssg(verbose, " - ", pkg)
      packages.to.install <- packages.to.install[!not.available]
    }
  } else {
    not.available <- character(0)
  }
  
  # install missing packages
  
  
  if(length(packages.to.install) > 0) {
    mssg(verbose, "Installing packages used in this project ")
    for(pkg in packages.to.install){
      if(pkg %in% unname(installed.packages()[, "Package"])) {
        mssg(verbose, " - Previously installed ", sQuote(pkg))
      } else {
        mssg(verbose, " - Installing ", sQuote(pkg))
        message(pkg)
        tryCatch({
          download_messages <- capture.output({ 
            suppressWarnings(
              utils::install.packages(pkgs = pkg, verbose = FALSE, 
                                      quiet = FALSE,
                                      INSTALL_opts = "--no-lock")
            )
          }, type = "message")
          checkpoint_log(
            download_messages,
            snapshotDate = snapshotDate,
            pkg,
            file = file.path(
              checkpointPath(snapshotDate, checkpointLocation, type = "root"),
              "checkpoint_log.csv")
          )
        }, error = function(e)e)
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
  
  z <- list(
    files_not_scanned = files.not.parsed,
    pkgs_found = packages.detected,
    pkgs_not_on_mran = names(not.available)[not.available],
    pkgs_installed = packages.to.install
  )
  invisible(z)}


#  ------------------------------------------------------------------------


setMranMirror <- function(
  snapshotDate, 
  snapshotUrl = checkpoint:::getSnapshotUrl(snapshotDate)){
  options(repos = snapshotUrl)}

setLibPaths <- function(checkpointLocation, libPath){
  assign(".lib.loc", c(libPath, 
                       checkpointBasePkgs(checkpointLocation)), 
         envir = environment(.libPaths))
}





mssg <- function(x, ...) if(x) message(...)

correctR <- function(x) compareVersion(as.character(utils::packageVersion("base")), x) == 0


# Scans for R files in a folder and the first level subfolders.
#
anyRfiles <- function(path = "."){
  findRfiles <- function(path = "."){
    pattern <- "\\.[rR]$|\\.[rR]nw$|\\.[rR]md$|\\.[rR]pres$|\\.[rR]proj$"
    z <- list.files(path = path, pattern = pattern, full.names = TRUE)
    normalizePath(z, winslash = "/")
  }
  dirs <- list.dirs(path = path, recursive = FALSE)
  rfiles <- as.vector(unlist(sapply(dirs, findRfiles)))
  length(rfiles) > 0
}

validateProjectFolder <- function(project) {
  if(normalizePath(project) == normalizePath("~/") && !anyRfiles(project)){
    message("This doesn't look like an R project directory.\n", 
            "Use forceProject = TRUE to force scanning"
    )
    answer = readline("Continue (y/n)? ")
    if(tolower(answer) != "y"){
      stop("Scanning stopped.", call. = FALSE)
    }
  }
}
