#' Install packages within an RRT repository.
#'
#' @keywords internal
#'
#' @param pkgs Vector of package names
#' @param repo A repository path. This is the path to the root of your RRT repository. By default,
#' we use the current working directory via \code{getwd()}.
#' @param repoid Repository ID.
#' @param lib Library path
#' @param suggests Install suggests or not. Default: FALSE.
#' @param verbose Print messages. Default: TRUE.

rrt_install2 <- function(pkgs=NULL, repo=getwd(), repoid, lib, mran=TRUE, suggests=FALSE, verbose=TRUE)
{
#   pkgslist <- paste0(lib, "/src/contrib/PACKAGES")
  pkgslist <- list.files(file.path(lib, "src/contrib"))

  mssg(verbose, "Looking for packages used in your repository...")
  x <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)

  if(length(pkgslist) == 0) {
    mssg(verbose, "Getting new packages...")
    pkgs2install <- getPkgs(x, repo, lib, verbose=verbose)
  } else {
    installedpkgs <- list.files(lib)
    installedpkgs <- installedpkgs[!installedpkgs %in% "src"]
    pkgs2install <- sort(x)[!sort(x) %in% sort(installedpkgs)]
  }
  
  if(!mran) pkgs2install <- c(pkgs2install, x)

  basepkgs <- rownames(installed.packages(priority="base"))
  pkgs2install <- pkgs2install[!pkgs2install %in% basepkgs]

  if(length(pkgs2install)==0){
    mssg(verbose, "No packages found to install")
  } else {
    mssg(verbose, "Installing packages...")
    allpkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
    names(allpkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
    allpkgs <- allpkgs[!grepl("PACKAGES", allpkgs)]
    pkgswithpath <- unname(sapply(pkgs2install, function(x) allpkgs[grepl(x, names(allpkgs))]))
    pkgswithpath <- pkgswithpath[!sapply(pkgswithpath, length) == 0]
    
    if(length(pkgswithpath) == 0){
      mssg(verbose, "No packages found to install")
    } else {
      # install github pkgs separately
      gh_install <- pkgswithpath[grep("\\.zip", pkgswithpath)]
      gh_install_names <- vapply(gh_install,
                                 function(x) sub("\\.zip", "", strsplit(x, '/')[[1]][length(strsplit(x, '/')[[1]])]),
                                 character(1), USE.NAMES = FALSE)
      for(i in seq_along(gh_install_names)){
        install_other(pkg=gh_install_names[i], lib=lib)
      }

      # regular install from tar.gz
      pkgswithpath <- pkgswithpath[!grepl("\\.zip", pkgswithpath)]
      pkgswithpath <- unlist(pkgswithpath)
      pkgswithpath <- normalizePath(pkgswithpath, winslash="/")
      lib <- normalizePath(lib)
      oldwd <- getwd()
      on.exit(setwd(oldwd))
      setwd(dirname(pkgswithpath[1]))
      install.packages(basename(pkgswithpath), lib = lib, repos=NULL, type = "source", dependencies=FALSE)
      
      # if any pkgs not installed, try installing from default CRAN mirror
      notinst <- pkgs2install[!vapply(file.path(lib, pkgs2install), file.exists, logical(1))]
      if(!length(notinst) == 0) install.packages(notinst, lib = lib, destdir = file.path(lib, "src/contrib"))
    }
  }
}


# not sure we need this to separate download from install from CRAN, since we can also put 
# source of each pkgs where we want using the destdir parameter. in `install.packages`
install_from_cran <- function(x){
  setwd(lib)
  on.exit(setwd(repo))
  dir.create("src/contrib", showWarnings = FALSE, recursive = TRUE)
#   get_github_pkgs(lib, pkgs2get, repo)  
  makeLibrary(pkgs = pkgs2get, path = file.path(lib, "src/contrib"))
}

#' is_bioc_pkg('stringr')
#' is_bioc_pkg(c('stringr','cowsay','dplyr'))
#' is_bioc_pkg(pkgs=c('stringr','ACME'))
#' is_bioc_pkg(pkgs=c('stringr','ACME','aroma.light'))
is_bioc_pkg <- function(pkgs){
  file <- file.path(tempdir(), "rrt_biockgs.rda")
  biocpkgs <- suppressWarnings(tryCatch(load(file), error = function(e) e))
  if(is(biocpkgs, "simpleError")){
    biocpkgs <- all_group()
    save(biocpkgs, file = file)
  } else { load(file) }
  vec <- pkgs %in% biocpkgs
  return( vec )
}

#' get_github_pkgs('cowsay')
get_github_pkgs <- function(repo, pkgs){
  githubpaths <- yaml.load_file(file.path(repo, "rrt/rrt_manifest.yml"))$Github
  toinstall <- sapply(pkgs, function(x) grep(x, githubpaths, value = TRUE), USE.NAMES = FALSE)
  for(i in seq_along(toinstall)){
    pathsplit <- strsplit(toinstall[i], "/")[[1]]
    RRT:::get_github(lib=lib, pkg=pathsplit[[2]], username=pathsplit[[1]])
  }
}

get_bioconductor_pkgs <- function(lib, pkgs, repo){
  biocpkgs <- all_group()
  pkgs_bioc <- pkgs[pkgs %in% biocpkgs]
  if(!length(pkgs_bioc) == 0){
    source("http://bioconductor.org/biocLite.R")
    BiocInstaller::biocLite(pkgs_bioc, lib = lib, destdir = file.path(lib, "src/contrib"), dependencies=FALSE, suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
  }
}