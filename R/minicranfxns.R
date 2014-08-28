# #' Retrieves package dependencies.
# #' 
# #' Performs recursive retrieve for \code{Depends}, \code{Imports} and \code{LinkLibrary}. Performs 
# #' non-recursive retrieve for \code{Suggests}.
# #'
# #' @keywords internal
# #'  
# #' @param pkg Character vector of packages.
# #' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using 
# #' \code{\link{available.packages}}
# #' @param repos URL(s) of the 'contrib' sections of the repositories. Passed to 
# #' \code{\link{available.packages}}
# #' @param type Passed to \code{\link{available.packages}}
# #' @param depends If TRUE, retrieves Depends, Imports and LinkingTo dependencies (non-recursively)
# #' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
# #' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
# #' @param path Destination download path
# #' @param includeBasePkgs If TRUE, include base R packages in results
# #' @param ... Other arguments passed to \code{\link{available.packages}}
# 
# pkgDep <- function(pkg, availPkgs, repos=getOption("repos"), type=getOption("pkgType"), 
#                    depends=TRUE, suggests=FALSE, enhances=FALSE, path, includeBasePkgs=FALSE, ...)
# {
#   if(!depends & !suggests & !enhances) {
#     warning("Returning nothing, since depends, suggests and enhances are all FALSE")
#     return(character(0))
#   }
#   
#   if(missing(pkg) || !is.character(pkg)){
#     stop("pkg should be a character vector with package names")
#   }
#   if(missing(availPkgs)){
#     if(!is.null(names(repos)) & repos["CRAN"] == "@CRAN@"){
#       repos <- c(CRAN="http://cran.revolutionanalytics.com")
#     }
#     if(is.na(type)) type <- "source"
#     availPkgs <- pkgAvail(path=path, repos=repos, type=type, ...)
#   }
#   if(nrow(availPkgs) == 0){
#     stop("Unable to retrieve available packages from CRAN")
#   }
#   
#   pkgInAvail <- pkg %in% availPkgs[, "Package"]
#   if(sum(pkgInAvail) == 0 ) stop("No valid packages in pkg")
#   if(sum(pkgInAvail) < length(pkg)){
#     warning("Package not recognized: ", paste(pkg[!pkgInAvail], collapse=", "))
#   }
#   
#   pkgAvail <- pkg[pkgInAvail]
#   
#   if(depends){
#     x <- tools::package_dependencies(pkgAvail, availPkgs, recursive=TRUE)
#     x1 <- unique(unname(unlist(x)))
#   } else {
#     x1 <- character(0)
#   }
#   if(suggests){
#     x <- tools::package_dependencies(pkgAvail, availPkgs, which="Suggests", recursive=FALSE)
#     x2 <- unique(unname(unlist(x)))
#   } else {
#     x2 <- character(0)
#   }
#   if(enhances){
#     x <- tools::package_dependencies(pkgAvail, availPkgs, which="Enhances", recursive=FALSE)
#     x3 <- unique(unname(unlist(x)))
#   } else {
#     x3 <- character(0)
#   }
#   ret <- sort(unique(c(pkgAvail, x1, x2, x3)))
#   if(!includeBasePkgs) ret <- ret[ret %in% rownames(availPkgs)]
#   ret
# }
# 
# #' Reads available packages from CRAN repository.
# #' 
# #' This is a thin wrapper around \code{\link{available.packages}}.  If the argument \code{path} is 
# #' supplied, then the function attempts to read from a local repository, otherwise attempts to read
# #' from a CRAN mirror at the \code{repos} url.
# #' 
# #' @keywords internal
# #' 
# #' @param repos URL(s) of the 'contrib' sections of the repositories. Passed to \code{\link{available.packages}}
# #' @param type Passed to \code{\link{available.packages}}
# #' @param ... Further args, not used
# 
# pkgAvail <- function(repos=getOption("repos"), type=getOption("pkgType"), ...)
# {
#   if(!grepl("^file", repos) & file.exists(repos)) {
#     repos <- paste0("file:///", repos)
#   } else {
#     if(!is.null(names(repos)) & repos["CRAN"] == "@CRAN@"){
#       repos <- c(CRAN="http://cran.revolutionanalytics.com")
#     }
#   }
#   available.packages(contrib.url(repos, type=type))
# }
# 
# 
# #' Downloads packages from CRAN to specified path and creates repository or library.
# #' 
# #' Given a list of packages, downloads to a specified destination folder, then creates PACKAGES 
# #' file.
# #' 
# #' The function \code{makeLibrary} downloads the packages into a single folder, i.e. similar to a 
# #' library on a machine.
# #' 
# #' Uses \code{\link{download.packages}} and \code{\link[tools]{write_PACKAGES}}
# #' 
# #' @keywords internal
# #' 
# #' @param pkgs Character vector of packages to download
# #' @param type Passed to \code{\link{download.packages}}
# #' @param path Destination download path
# 
# makeLibrary <- function(pkgs, path, type="source")
# {
#   if(!file.exists(path)) stop("Download path does not exist")
#   wd <- getwd()
#   on.exit(setwd(wd))
#   setwd(normalizePath(path))
#   message(getwd())
#   download.packages(pkgs, destdir=path, type=type)
# }