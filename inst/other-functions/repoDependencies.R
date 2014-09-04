# #' Get repository dependencies
# #'
# #' This function first determines packages used in the repo, then recursively gets dependencies of the packages you use (for Depends, Imports and LinkLibrary only, see \code{?pkgDep}).  Optionally, you can get dependencies for Suggests and Enhances (non-recursively). NOTE: Enhances not working right now.
# #'
# #' @import yaml httr
# #' @export
# #'
# #' @param repo (character) A path to a RRT repository; defaults to current working directory.
# #' @param simplify (logical) If TRUE, simplify list to a vector with all unique packages.
# #' @param base (logical) If TRUE, return base R packages, if FALSE, don't return them.
# #' @param suggests (logical) Download and install packages in the Suggests line for packages
# #' used in your RRT repository, or not. Default: FALSE.
# #' @param ... Further args passed on to \code{miniCRAN::pkgDep}
# #'
# #' @keywords internal  
# #' @return A named list of packages, named by the package that requires said dependencies
# #' 
# repoDependencies <- function(pkgs, simplify=FALSE, base=TRUE, suggests=FALSE, ...) {
#   # remove RRT, manipulate, rstudio, and packrat
#   pkgs_used <- setdiff(pkgs, c('RRT','manipulate','packrat','rstudio'))
#   
#   # Get package dependencies using miniCRAN
#   pkg_deps <- lapply(pkgs_used, pkgDep_try, repo=repo, suggests=suggests)
#   names(pkg_deps) <- pkgs_used
#   
#   if(simplify){
#     allpkgs <- unname(do.call(c, pkg_deps))
#     pkg_deps <- unique(allpkgs)
#   }
#   
#   # remove base R pkgs
#   if(!base){
#     availpkgs <- available.packages(contrib.url(getOption("repos"), "source"))
#     pkg_deps <- pkg_deps[!sapply(pkg_deps, function(x){
#       zz <- tryCatch(availpkgs[x,]["Priority"], error=function(e) e)
#       if("error" %in% class(zz)) { FALSE } else {
#         if(!is.na(zz[['Priority']]) && zz[['Priority']] == "base") TRUE else FALSE
#       }
#     })]
#     basePackages <- c('base','compiler','datasets','graphics','grDevices','grid','methods','parallel',
#                       'splines','stats','stats4','tcltk','tools','utils')
#     pkg_deps <- setdiff(pkg_deps, basePackages)
#   }
#   
#   pkg_deps
# }
# 
# 
# #' @importFrom miniCRAN pkgDep
# pkgDep_try <- function(x, repo=NULL, suggests=FALSE){
#   tmp <- tryCatch(miniCRAN::pkgDep(x, type="source", suggests = suggests), error=function(e) e)
#   if(!"error" %in% class(tmp)){ tmp } else {
#     pkg_deps_noncran(repo, x)
#   }
# }
# 
# 
# pkg_deps_noncran <- function(repo, x){
#   if(is.biocPackage(x)){
#     pkgDep(x, repos = c(CRAN=biocinstallRepos(siteRepos=character())[[1]]))
#   } else {
#     # look for user specified manifest file, and read from there if any
#     userManifestFile <- file.path(repo, "manifest.yml")
#     
#     # look for package mention in manifest, return message if not
#     if(!file.exists(userManifestFile)){ 
#       out <- "not found" 
#     } else {
#       tt <- suppressWarnings(yaml.load_file(userManifestFile))
#       nn <- tt[names(tt) %in% c("Github","MRAN","Bitbucket","Bioconductor","Gitorious")]
#       trymatch <- lapply(nn, function(y) y[sapply(y, function(z) grepl(x, z))])
#       if(all(sapply(trymatch, length)==0)){ out <- "not found" } else {
#         out <- trymatch[!sapply(trymatch, length)==0]
#       }
#     }
#     
#     if(!out == "not found"){
#       from <- tolower(names(out))
#       from <- match.arg(from, c('github','mran','bitbucket','bioconductor','gitorious'))
#       switch(from,
#              github = c(x, get_desc_github(out[[1]])))
#     } else {
#       sprintf("%s not found - make sure to specify info in the manifest file at %s", x, userManifestFile)
#     }
#   }
# }
# 
# 
# #' @importFrom RCurl base64Decode
# get_desc_github <- function(userrepo, depends=TRUE, suggests=FALSE, enhances=FALSE){
#   #   GET /repos/:owner/:repo/contents/:path
#   url <- "https://api.github.com/repos/%s/%s/contents/DESCRIPTION"
#   ur <- strsplit(userrepo, "/")[[1]]
#   url <- sprintf(url, ur[[1]], ur[[2]])
#   res <- GET(url)
#   if(res$headers$statusmessage == "OK"){
#     tt <- content(res, as = "parsed")$content
#     txt <- paste(lapply(strsplit(tt, "\n")[[1]], RCurl::base64Decode, mode="character"), collapse = "")
#     fields <- suppressWarnings(yaml.load(txt))
#     out <- if(depends) c(fields[['Depends']], fields[['Imports']])
#     out <- if(suggests) c(out, fields[['Suggests']]) else out
#     out <- if(enhances) c(out, fields[['Enhances']]) else out
#     tmp <- as.vector(sapply(out, function(bb) gsub("\\s", "", strsplit(bb, ",")[[1]]), USE.NAMES = FALSE))
#     tmp2 <- do.call(c, as.list(tmp))
#     tmp3 <- parse_pkg_ver(tmp2)
#     # remove R
#     # NOTE: parse_pkg_ver also provid2es with any version specification, so use that later
#     tmp4 <- tmp3[!sapply(tmp3, function(w) any(w %in% "R"))]
#     vapply(tmp4, function(x) x[[1]], "")
#   } else { "No DESCRIPTION file found" }
# }
# 
# 
# parse_pkg_ver <- function(input){
#   lapply(input, function(v){
#     tmp <- gsub("[)]", "", strsplit(v, "[(]")[[1]])
#     if(length(tmp)==1) c(tmp, "NA") else tmp
#   })
# }
