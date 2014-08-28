#' Install packages within an RRT repository.
#' 
#' @import BiocInstaller
#' @keywords internal
#'
#' @param repo A repository path. This is the path to the root of your RRT repository. By default, we use the current working directory via \code{\link{getwd}}.
#' @param repoid Repository ID.
#' @param lib Library path
#' @param suggests Install suggests or not. Default: FALSE.
#' @param verbose Print messages. Default: TRUE
#' @param quiet Passed to \code{\link[utils]{install.packages}}

rrt_install <- function(repo=getwd(), repoid, lib=rrt_libpath(repo), mran=TRUE, suggests=FALSE, verbose=TRUE, quiet=FALSE)
{
  mssg(verbose, "Downloading packages used in your repository...")
  x <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)
  
  # make src/contrib
  suppressWarnings(dir.create(file.path(lib, "src/contrib"), recursive = TRUE))
  
  if(length(x) > 0) {
    installedpkgs <- list.files(lib)
    installedpkgs <- installedpkgs[!installedpkgs %in% "src"]
    p2inst <- sort(x)[!sort(x) %in% sort(installedpkgs)]
    
    cranpkgs <- p2inst[is_cran_pkg(p2inst)]
    biocPkgs <- p2inst[is_bioc_pkg(p2inst)]
    pkgsrem <- p2inst[!p2inst %in% c(cranpkgs, biocPkgs)]
    if(length(pkgsrem) > 0) {
      get_github_pkgs(repo, pkgs = pkgsrem, lib)
      githubpkgs <- pkgsrem
    } else { githubpkgs <- character(0) }
    
    if(mran){
      pkgs_mran_get(lib, repo, cranpkgs, quiet=quiet)
      install_mran_pkgs(lib, cranpkgs, verbose=verbose, quiet=quiet)
    } else {
      utils::install.packages(cranpkgs, lib = lib, destdir = file.path(lib, "src/contrib"), quiet=quiet, verbose=verbose)
    }
    
    # check for any failed intalls and install from binary from default CRAN mirror
    notinst <- cranpkgs[!vapply(file.path(lib, cranpkgs), file.exists, logical(1))]
    if(!length(notinst) == 0) {
      mssg(verbose, "... Installing from default CRAN mirror")
      utils::install.packages(notinst, lib = lib, destdir = file.path(lib, "src/contrib"), quiet=quiet, verbose=verbose)
    }
    
    # install github pkgs
    if(!length(githubpkgs) == 0) {
      mssg(verbose, "... Installing from github")
      install_github_pkgs(lib, githubpkgs)
    }
    
    # install (and download bioc pkgs)
    if(!length(biocPkgs) == 0) {
      mssg(verbose, "... Installing from BioConductor")
      get_bioconductor_pkgs(lib, biocPkgs, repo)
    }
  } else {
    mssg(verbose, "... nothing to install")
  }
}

install_mran_pkgs <- function(lib, yyy, verbose, quiet=FALSE){
  if(length(yyy)==0){
    mssg(verbose, "... No MRAN packages found to install")
  } else {
    mssg(verbose, "Installing packages...")
    allPkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
    names(allPkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
    allPkgs <- allPkgs[!grepl("PACKAGES", allPkgs)]
    pkgsWithPath <- unname(sapply(yyy, function(x) allPkgs[grepl(x, names(allPkgs))]))
    pkgsWithPath <- pkgsWithPath[!sapply(pkgsWithPath, length) == 0]
    
    if(length(pkgsWithPath) == 0){
      mssg(verbose, "... No MRAN packages found to install")
    } else {
      pkgsWithPath <- pkgsWithPath[!grepl("\\.zip", pkgsWithPath)]
      pkgsWithPath <- unlist(pkgsWithPath)
      pkgsWithPath <- normalizePath(pkgsWithPath, winslash="/")
      lib <- normalizePath(lib)
      oldwd <- getwd()
      setwd(dirname(pkgsWithPath[1]))
      on.exit(setwd(oldwd))
      utils::install.packages(basename(pkgsWithPath), lib = lib, repos=NULL, 
                       type = "source", dependencies=FALSE, quiet=quiet)
    }
  }
}

pkgs_mran_get <- function(lib, repo, pkgs2get, quiet=FALSE){
  pkgloc <- file.path(lib, "src/contrib")
  setwd(lib)
  on.exit(setwd(repo))
  suppressWarnings(dir.create(file.path(lib, "src/contrib"), recursive = TRUE))
  download_pkgs_mran(repo=repo, lib=lib, snapshotid = getOption('RRT_snapshotID'), 
                     pkgs=pkgs2get, outdir=pkgloc, quiet=quiet)
}


is_cran_pkg <- function (pkgs, repos = c(CRAN="http://cran.r-project.org/"), type = "source"){
  if (!grepl("^file", repos) && file.exists(repos)) {
    repos <- paste0("file:///", repos)
  }
  tt <- available.packages(contrib.url(repos, type = type))
  availCranPkgs <- row.names(tt)
  pkgs %in% availCranPkgs
}

#' Determine if a package is on Bioconductor or not.
#' @keywords internal
#' @param pkgs Vector of package names
#' @return A logical vector of same length as input vector
is_bioc_pkg <- function(pkgs){
  file <- file.path(tempdir(), "rrt_biockgs.rda")
  biocPkgs <- suppressWarnings(tryCatch(load(file), error = function(e) e))
  if(is(biocPkgs, "simpleError")){
    biocPkgs <- all_group()
    save(biocPkgs, file = file)
  } else { load(file) }
  pkgs %in% biocPkgs
}

#' Download github packages.
#' @keywords internal
#' @param repo Repository path
#' @param pkgs Vector of package names
#' @param lib Library path
get_github_pkgs <- function(repo, pkgs, lib){
  mssg(TRUE, "get_github_packages in rrt_install.R")
  yaml_file <- file.path(repo, "manifest.yml")
  githubPaths <- if(file.exists(yaml_file)){
    yaml.load_file(yaml_file)$Github
  } else NULL
  if(is.null(githubPaths)) { character(0) } else {
    toinstall <- sapply(pkgs, function(x) grep(x, githubPaths, value = TRUE), USE.NAMES = FALSE)
    for(i in seq_along(toinstall)){
      pathsplit <- strsplit(toinstall[i], "/")[[1]]
      get_github(pkg=pathsplit[[2]], username=pathsplit[[1]], lib=lib)
    }
  }
}

install_github_pkgs <- function(lib, pkgs){
  allPkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
  names(allPkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
  allPkgs <- allPkgs[!grepl("PACKAGES", allPkgs)]
  pkgsWithPath <- unname(sapply(pkgs, function(x) allPkgs[grepl(x, names(allPkgs))]))
  pkgsWithPath <- pkgsWithPath[!sapply(pkgsWithPath, length) == 0]
  
  gh_install <- pkgsWithPath[grep("\\.zip", pkgsWithPath)]
  zipFunction <- function(x) {
    splitx <- strsplit(x, '/')[[1]]
    sub("\\.zip", "", splitx[length(splitx)])
  }
  gh_install_names <- vapply(gh_install,
                             zipFunction,
                             character(1), USE.NAMES = FALSE)
  for(i in seq_along(gh_install_names)){
    install_other(pkg=gh_install_names[i], lib=lib)
  }
}

get_bioconductor_pkgs <- function(lib, pkgs, repo){
  biocPkgs <- all_group()
  pkgs_bioc <- pkgs[pkgs %in% biocPkgs]
  if(!length(pkgs_bioc) == 0){
    source("http://bioconductor.org/biocLite.R")
    BiocInstaller::biocLite(pkgs_bioc, lib = lib, destdir = file.path(lib, "src/contrib"), dependencies=FALSE, suppressUpdates = TRUE, suppressAutoUpdate = TRUE)
  }
}