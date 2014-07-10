#' Get repository dependencies
#' 
#' This function first determines packages used in the repo, then recursively gets dependencies
#' of the packages you use (for Depends, Imports and LinkLibrary only, see \code{?pkgDep}). 
#' Optionally, you can get dependencies for Suggests and Enhances (non-recursively). NOTE: 
#' Enhances not working right now.
#'
#' @import miniCRAN yaml httr
#' @export
#' 
#' @param repo (character) A path to a RRT repository; defaults to current working directory.
#' @param simplify (logical) If TRUE, simplify list to a vector with all unique packages.
#' @param base (logical) If TRUE, return base R packages, if FALSE, don't return them.
#' @param suggests (logical) Download and install packages in the Suggests line for packages
#' used in your RRT repository, or not. Default: FALSE.
#' @param ... Further args passed on to \code{miniCRAN::pkgDep}
#' 
#' @keywords internal
#' @return A named list of packages, named by the package that requires said dependencies
#' @examples \dontrun{
#' repodeps(repo="~/newrepo")
#' repodeps(repo="~/newrepo", simplify=TRUE)
#' 
#' # defaults to working directory
#' setwd("~/newrepo")
#' repodeps() 
#' 
#' # Get Suggests too, not retrieved by default
#' repodeps(repo="~/newrepo", simplify=TRUE, suggests=TRUE)
#' }

repodeps <- function(repo=getwd(), simplify=FALSE, base=TRUE, suggests=FALSE, ...)
{
  # Get packages used in the repository
  pkgs_used <- rrt_deps(repo) 
  # remove RRT
  pkgs_used <- pkgs_used[!pkgs_used %in% 'RRT']
  
  # Get package dependencies using miniCRAN
  pkg_deps <- lapply(pkgs_used, pkgDep_try, repo=repo, suggests=suggests, ...)
  names(pkg_deps) <- pkgs_used
  
  if(simplify){
    allpkgs <- unname(do.call(c, pkg_deps))
    pkg_deps <- unique(allpkgs)
  }
  
  # remove base R pkgs
  if(!base){
    availpkgs <- available.packages(contrib.url(getOption("repos"), "source"))
    pkg_deps <- pkg_deps[!sapply(pkg_deps, function(x){
      zz <- tryCatch(availpkgs[x,]["Priority"], error=function(e) e)
      if("error" %in% class(zz)) { FALSE } else {
        if(!is.na(zz[['Priority']]) && zz[['Priority']] == "base") TRUE else FALSE
      }
    })]
    pkg_deps <- pkg_deps[!pkg_deps %in% 
          c('base','compiler','datasets','graphics','grDevices','grid','methods','parallel',
            'splines','stats','stats4','tcltk','tools','utils')]
  }
  
  return(pkg_deps)
}

pkgDep_try <- function(x, repo=NULL, ...){
  # FIXME - currently only works for Github
  tmp <- tryCatch(pkgDep(x, ...), error=function(e) e)
  if(!"error" %in% class(tmp)){ tmp } else {
    pkg_deps_noncran(repo, x)
  }
}

pkg_deps_noncran <- function(repo, x){
  ### FIXME: if we index non-CRAN packages on MRAN, we could easily search MRAN instead of the mess below
  manfile <- file.path(repo, "rrt/rrt_manifest.yml")
  
  # look for user specified manifest file, and read from there if any
  usermanfile <- file.path(repo, "manifest.yml")
  if(!file.exists(usermanfile)){ userdeets <- NULL } else {
    tt <- suppressWarnings(yaml.load_file(usermanfile))
    if(!is.null(tt)){
      nn <- tt[names(tt) %in% c("Github","MRAN","Bitbucket","Bioconductor","Gitorious")]
      trymatch <- lapply(nn, function(y) y[sapply(y, function(z) grepl(x, z))])
      if(all(sapply(trymatch, length)==0)){ out <- "not found" } else {
        userdeets <- trymatch[!sapply(trymatch, length)==0]
        uu <- list()
        for(i in seq_along(userdeets)){
          uu[[i]] <- paste(sprintf(" - %s", userdeets[[i]]))
        }
        github <- sprintf("Github:\n %s", uu)
      }
      if(file.exists(manfile)){
        manfiletmp <- suppressWarnings(yaml.load_file(manfile))
        if("Github" %in% names(manfiletmp)){
          if(any(grepl(x, manfiletmp$Github))){ NULL } else {
            cat(github, file = manfile, append = TRUE)
          }
        } else {
          cat(github, file = manfile, append = TRUE)
        }
      }
    }
  }
  
  # look for package mention in manifest, return message if not
  if(!file.exists(manfile)){ out <- "not found" } else {  
    tt <- suppressWarnings(yaml.load_file(manfile))
    nn <- tt[names(tt) %in% c("Github","MRAN","Bitbucket","Bioconductor","Gitorious")]
    trymatch <- lapply(nn, function(y) y[sapply(y, function(z) grepl(x, z))])
    if(all(sapply(trymatch, length)==0)){ out <- "not found" } else {
      out <- trymatch[!sapply(trymatch, length)==0]
    }
  }
  
  if(!out == "not found"){
    from <- tolower(names(out))
    from <- match.arg(from, c('github','mran','bitbucket','bioconductor','gitorious'))
    switch(from, 
           github = c(x, get_desc_github(out[[1]])))
  } else {
    sprintf("%s not found - make sure to specify info in the manifest file at %s", x, usermanfile)
  }
}

get_desc_github <- function(userrepo, depends=TRUE, suggests=FALSE, enhances=FALSE){
#   GET /repos/:owner/:repo/contents/:path
  url <- "https://api.github.com/repos/%s/%s/contents/DESCRIPTION"
  ur <- strsplit(userrepo, "/")[[1]]
  url <- sprintf(url, ur[[1]], ur[[2]])
  res <- GET(url)
  if(res$headers$statusmessage == "OK"){
    tt <- content(res, as = "parsed")$content
    txt <- paste(lapply(strsplit(tt, "\n")[[1]], RCurl::base64Decode, mode="character"), collapse = "")
    fields <- suppressWarnings(yaml.load(txt))
    out <- if(depends) c(fields[['Depends']], fields[['Imports']])
    out <- if(suggests) c(out, fields[['Suggests']]) else out
    out <- if(enhances) c(out, fields[['Enhances']]) else out
    tmp <- as.vector(sapply(out, function(bb) gsub("\\s", "", strsplit(bb, ",")[[1]]), USE.NAMES = FALSE))
    tmp2 <- do.call(c, as.list(tmp))
    tmp3 <- parse_pkg_ver(tmp2)
    # remove R
    # NOTE: parse_pkg_ver also provides with any version specification, so use that later
    tmp4 <- tmp3[!sapply(tmp3, function(w) any(w %in% "R"))]
    vapply(tmp4, function(x) x[[1]], "")
  } else { "No DESCRIPTION file found" }
}

parse_pkg_ver <- function(input){
  lapply(input, function(v){
    tmp <- gsub("[)]", "", strsplit(v, "[(]")[[1]])
    if(length(tmp)==1) c(tmp, "NA") else tmp
  })
}