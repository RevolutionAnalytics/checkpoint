#' Install packages within an RRT repository.
#' 
#' @import BiocInstaller
#' @keywords internal
#'
#' @param repo A repository path. This is the path to the root of your RRT repository. By default,
#' we use the current working directory via \code{getwd()}.
#' @param repoid Repository ID.
#' @param lib Library path
#' @param suggests Install suggests or not. Default: FALSE.
#' @param verbose Print messages. Default: TRUE.

rrt_install3 <- function(repo=getwd(), repoid, lib, mran=TRUE, suggests=FALSE, verbose=TRUE)
{
#   pkgslist <- list.files(file.path(lib, "src/contrib"))  
  mssg(verbose, "Looking for packages used in your repository...")
  x <- repodeps(repo, simplify = TRUE, base=FALSE, suggests=suggests)
  
  # make src/contrib
  suppressWarnings(dir.create(file.path(lib, "src/contrib"), recursive = TRUE))
  
  if(length(x) > 0) {
    installedpkgs <- list.files(lib)
    installedpkgs <- installedpkgs[!installedpkgs %in% "src"]
    p2inst <- sort(x)[!sort(x) %in% sort(installedpkgs)]
    
    cranpkgs <- p2inst[is_cran_pkg(p2inst)]
    biocpkgs <- p2inst[is_bioc_pkg(p2inst)]
    pkgsrem <- p2inst[!p2inst %in% c(cranpkgs, biocpkgs)]
    if(length(pkgsrem) > 0) {
      get_github_pkgs(repo, pkgs = pkgsrem, lib)
      githubpkgs <- pkgsrem
    } else { githubpkgs <- character(0) }
    
    if(mran){
      pkgs_mran_get(lib, repo, cranpkgs)
      install_mran_pkgs(lib, cranpkgs, verbose)
    } else {
      install.packages(cranpkgs, lib = lib, destdir = file.path(lib, "src/contrib"))
    }
    
    # install github pkgs
    if(!length(githubpkgs) == 0) install_github_pkgs(lib, githubpkgs)
    
    # install (and download bioc pkgs)
    if(!length(biocpkgs) == 0) get_bioconductor_pkgs(lib, biocpkgs, repo)
  } else {
    mssg(verbose, "nothing to install")
  }
}

install_mran_pkgs <- function(lib, yyy, verbose){
#   basepkgs <- rownames(installed.packages(priority="base"))
#   yyy <- yyy[!yyy %in% basepkgs]
#   
  if(length(yyy)==0){
    mssg(verbose, "No packages found to install")
  } else {
    mssg(verbose, "Installing packages...")
    allpkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
    names(allpkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
    allpkgs <- allpkgs[!grepl("PACKAGES", allpkgs)]
    pkgswithpath <- unname(sapply(yyy, function(x) allpkgs[grepl(x, names(allpkgs))]))
    pkgswithpath <- pkgswithpath[!sapply(pkgswithpath, length) == 0]
    
    if(length(pkgswithpath) == 0){
      mssg(verbose, "No packages found to install")
    } else {
      pkgswithpath <- pkgswithpath[!grepl("\\.zip", pkgswithpath)]
      pkgswithpath <- unlist(pkgswithpath)
      pkgswithpath <- normalizePath(pkgswithpath, winslash="/")
      lib <- normalizePath(lib)
      oldwd <- getwd()
      setwd(dirname(pkgswithpath[1]))
      on.exit(setwd(oldwd))
      install.packages(basename(pkgswithpath), lib = lib, repos=NULL, type = "source", dependencies=FALSE)
    }
  }
}

pkgs_mran_get <- function(lib, repo, pkgs2get){
  pkgloc <- file.path(lib, "src/contrib")
  setwd(lib)
  on.exit(setwd(repo))
  suppressWarnings(dir.create(file.path(lib, "src/contrib"), recursive = TRUE))
  pkgs_mran(repo=repo, lib=lib, snapshotid = getOption('RRT_snapshotID'), pkgs=pkgs2get, outdir=pkgloc)
}

# not sure we need this to separate download from install from CRAN, since we can also put 
# source of each pkgs where we want using the destdir parameter. in `install.packages`
# install_from_cran <- function(x){
#   setwd(lib)
#   on.exit(setwd(repo))
#   dir.create("src/contrib", showWarnings = FALSE, recursive = TRUE)
#   #   get_github_pkgs(lib, pkgs2get, repo)  
#   makeLibrary(pkgs = pkgs2get, path = file.path(lib, "src/contrib"))
# }

is_cran_pkg <- function (pkgs, repos = c(CRAN="http://cran.r-project.org/"), type = "source"){
  if (!grepl("^file", repos) && file.exists(repos)) {
    repos <- paste0("file:///", repos)
  }
  tt <- available.packages(contrib.url(repos, type = type))
  availcranpkgs <- row.names(tt)
  pkgs %in% availcranpkgs
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
  pkgs %in% biocpkgs
}

#' get_github_pkgs('cowsay')
get_github_pkgs <- function(repo, pkgs, lib){
  githubpaths <- yaml.load_file(file.path(repo, "manifest.yml"))$Github
  if(is.null(githubpaths)) { character(0) } else {
    toinstall <- sapply(pkgs, function(x) grep(x, githubpaths, value = TRUE), USE.NAMES = FALSE)
    for(i in seq_along(toinstall)){
      pathsplit <- strsplit(toinstall[i], "/")[[1]]
      get_github(pkg=pathsplit[[2]], username=pathsplit[[1]], lib=lib)
    }
  }
}

install_github_pkgs <- function(lib, yyy){
  allpkgs <- list.files(file.path(lib, "src/contrib"), full.names = TRUE)
  names(allpkgs) <- gsub("_[0-9].+", "", list.files(file.path(lib, "src/contrib")))
  allpkgs <- allpkgs[!grepl("PACKAGES", allpkgs)]
  pkgswithpath <- unname(sapply(yyy, function(x) allpkgs[grepl(x, names(allpkgs))]))
  pkgswithpath <- pkgswithpath[!sapply(pkgswithpath, length) == 0]
  
  gh_install <- pkgswithpath[grep("\\.zip", pkgswithpath)]
  gh_install_names <- vapply(gh_install,
                             function(x) sub("\\.zip", "", strsplit(x, '/')[[1]][length(strsplit(x, '/')[[1]])]),
                             character(1), USE.NAMES = FALSE)
  for(i in seq_along(gh_install_names)){
    install_other(pkg=gh_install_names[i], lib=lib)
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