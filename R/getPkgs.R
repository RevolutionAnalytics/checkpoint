#' Function to install pkgs
#'
#' @import miniCRAN
#' 
#' @param x (character) A vector of package names. If NULL, none installed, and message prints
#' @param lib (character) Library location, a directory
#' @param recursive (logical) Recursively install packages?
#' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
#' @param install (logical) Install packages or just download packages. Not used yet...
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See 
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of snapshot to use. E.g. "2014-06-20"
#' 
#' @keywords internal
#' 
#' @examples \dontrun{
#' getPkgs()
#' }
getPkgs <- function(x, lib, recursive=FALSE, verbose=TRUE, install=TRUE, mran=FALSE, snapdate=NULL){
  # check for existence of pkg, subset only those that need to be installed
  if(is.null(x)){ NULL } else {
    
    pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
    if(!file.exists(pkgslist)) { pkgs2install <- x } else {
      installedpkgs <- gsub("Package:\\s", "", grep("Package:", readLines(pkgslist), value=TRUE))
      pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
    }
    
    # Make local repo of packages
    if(!is.null(pkgs2install) || length(pkgs2install) == 0){
      if(!mran){
        # FIXME, needs some fixes on miniCRAN to install source if binaries not avail.-This may be fixed now
        makeRepo(pkgs = pkgs2install, path = lib, download = TRUE)
      } else {
        if(is.null(snapdate)) snapdate <- Sys.Date()
        snapdate <- "2014-06-19"
        #         snapdate <- as.POSIXct(Sys.Date())
        #         format(snapdate, tz="Europe/London", usetz=TRUE)
        pkgloc <- file.path(lib, "src/contrib")
        setwd(lib)
        dir.create("src/contrib", recursive = TRUE)
        pkgs_mran(date=snapdate, pkgs=pkgs2install, outdir=pkgloc)
      }
    } else {
      return(mssg(verbose, "No packages found - none installed"))
    }
  }
}