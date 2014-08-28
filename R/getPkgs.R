#' Function to download packages
#'
#' @param x (character) A vector of package names. If NULL, none installed, and message prints
#' @param repo Repository path
#' @param lib (character) Library location, a directory
#' @param recursive (logical) Recursively install packages?
#' @param verbose (logical) Inherited from call to rrt_init or rrt_refresh
#' @param install (logical) Install packages or just download packages. Not used yet...
#' @param mran (logical) If TRUE, packages are installed from the MRAN server. See
#' \url{http://mran.revolutionanalytics.com} for more information.
#' @param snapdate Date of MRAN snapshot to use. E.g. "2014-06-20"
#' @param snapshotid MRAN snapshotID to use. E.g. "2014-06-30_1700"
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' getPkgs("<path to RRT repo>")
#' }

getPkgs <- function(x, repo, lib, recursive=FALSE, verbose=TRUE, install=TRUE, mran=FALSE){
  # check for existence of pkg, subset only those that need to be installed
  mssg(verbose, "... running getPkgs()")
  if(is.null(x)){ NULL } else {

    pkgslist <- list.files(file.path(lib, "src/contrib"))
    if(length(pkgslist) == 0) { pkgs2get <- x } else {
      gotpkgs <- vapply(pkgslist, function(x) strsplit(x, "_")[[1]][[1]], character(1), USE.NAMES = FALSE)
      pkgs2get <- sort(x)[!sort(x) %in% sort(gotpkgs)]
    }

    # Make local repo of packages
    if(!is.null(pkgs2get) || length(pkgs2get) == 0){
      if(!mran){
        NULL
      } else {
        pkgloc <- file.path(lib, "src/contrib")
        setwd(lib)
        on.exit(setwd(repo))
        dir.create("src/contrib", showWarnings = FALSE, recursive = TRUE)
        snapdateid <- getOption('RRT_snapshotID')
        download_pkgs_mran(repo=repo, lib=lib, snapshotid = snapdateid, pkgs=pkgs2get, outdir=pkgloc)
      }
      
      # download pkgs from Github
      get_github_pkgs(lib, pkgs2get, repo)
    } else {
      return(mssg(verbose, "No packages found - none downloaded"))
    }
  }
}

get_github_pkgs <- function(lib, pkgs, repo){
  mssg(TRUE, "get_github_packages in getPkgs.R")
  availcranpkgs <- row.names(availCRANpkgs())
  notoncran <- pkgs[!pkgs %in% availcranpkgs]
  message(sprintf("Not found on CRAN:\n%s", paste0(notoncran, collapse = ", ")))
  githubpaths <- yaml.load_file(file.path(repo, "rrt/rrt_manifest.yml"))$Github
  toinstall <- sapply(notoncran, function(x) grep(x, githubpaths, value = TRUE), USE.NAMES = FALSE)
  for(i in seq_along(toinstall)){
    pathsplit <- strsplit(toinstall[i], "/")[[1]]
    get_github(lib=lib, pkg=pathsplit[[2]], username=pathsplit[[1]])
  }
}

availCRANpkgs <- function (repos = getOption("repos"), type = getOption("pkgType"), ...){
  if (!grepl("^file", repos) && file.exists(repos)) {
    repos <- paste0("file:///", repos)
  }
  available.packages(contrib.url(repos, type = type))
}