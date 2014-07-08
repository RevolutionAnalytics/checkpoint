#' Function to download packages
#'
#' @import miniCRAN
#'
#' @param x (character) A vector of package names. If NULL, none installed, and message prints
#' @param repo Repository path
#' @param lib (character) Library location, a directory
#' @param recursive (logical) Recursively install packages?
#' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
#' @param install (logical) Install packages or just download packages. Not used yet...
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See
#' \url{http://marmoset.revolutionanalytics.com/} for more information.
#' @param snapdate Date of MRAN snapshot to use. E.g. "2014-06-20"
#' @param snapshotid MRAN snapshotID to use. E.g. "2014-06-30_1700"
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' getPkgs("<path to RRT repo>")
#' }

getPkgs <- function(x, repo, lib, recursive=FALSE, verbose=TRUE, install=TRUE, mran=FALSE, snapdate=NULL, snapshotid=NULL){
  # check for existence of pkg, subset only those that need to be installed
  mssg(verbose, "... running getPkgs()")
  if(is.null(x)){ NULL } else {

    pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
    if(!file.exists(pkgslist)) { pkgs2get <- x } else {
      gotpkgs <- gsub("Package:\\s", "", grep("Package:", readLines(pkgslist), value=TRUE))
      pkgs2get <- sort(x)[!sort(x) %in% sort(gotpkgs)]
    }

    # Make local repo of packages
    if(!is.null(pkgs2get) || length(pkgs2get) == 0){
      if(!mran){
        # FIXME, needs some fixes on miniCRAN to install source if binaries not avail.-This may be fixed now
        makeRepo(pkgs = pkgs2get, path = lib, download = TRUE)
        options(RRT_snapshotID = "none")
      } else {
        snapdateid <- if(is.null(snapshotid)){
          if(is.null(snapdate)) snapdate <- Sys.Date()
          getsnapshotid(snapdate)
        } else { snapshotid }
        pkgloc <- file.path(lib, "src/contrib")
        setwd(lib)
        on.exit(setwd(repo))
        dir.create("src/contrib", showWarnings = FALSE, recursive = TRUE)
        pkgs_mran(snapshotid = snapdateid, pkgs=pkgs2get, outdir=pkgloc)
        options(RRT_snapshotID = snapdateid)
      }
    } else {
      return(mssg(verbose, "No packages found - none downloaded"))
    }
  }
}
