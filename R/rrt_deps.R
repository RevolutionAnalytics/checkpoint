#' Detect repository dependencies
#'
#' Recursively detect all repository dependencies. Parses all .R, .Rmd, .Rnw, .Rpres, and other 
#' files in the repository directory to determine what packages the repo depends directly.
#'
#' Only direct dependencies are detected (i.e. no recursion).
#' 
#' NOTE: Modified from dependencies.R in the packrat github repo
#' NOTE: Working on adding support for other file types, including .md
#'
#' @import knitr
#' @export
#' @keywords internal
#' 
#' @param repo Repository path. Defaults to current working directory.
#' @param fileext (character) File extensions to search for. Default is .R, .Rmd, .Rnw, and .Rpres. 
#'    Specify a certain file extension(s) in a charcter vector. Exclude a file extension by e.g. 
#'    "-.Rmd"
#' @param verbose (logical) Print messages or not.
#' 
#' @return Vector of package names on which R code in the repository depends.
#' @details Dependencies are determined by parsing repository source code and
#'   looking for calls to \code{library}, \code{require}, \code{::}, and
#'   \code{:::}.
#'
#' @examples \dontrun{
#' # dependencies for the repo in the current working dir
#' rrt_deps()
#'
#' # dependencies for an repo in another directory
#' rrt_deps("~/newrepo")
#' 
#' # include only certain file extensions
#' rrt_deps(fileext=c('Rmd'))
#' 
#' # exclude some file extensions
#' rrt_deps(fileext=c('-Rmd'))
#' rrt_deps(fileext=c('-Rmd','Rnw'))
#' 
#' # suppress messages
#' rrt_deps(verbose=FALSE)
#' }
rrt_deps <- function(repo = NULL, fileext = NULL, verbose = TRUE){
  if(is.null(repo)) repo <- getwd()
  repo <- path.expand(repo)
  unique(repo_deps(dir = repo))
}

# detect all package dependencies for a repo
repo_deps <- function(dir) {
  dir <- normalizePath(dir, winslash='/')
  #   pattern <- "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$|\\.txt$|\\.md$"
  pattern <- "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$"
  R_files <- list.files(dir, pattern = pattern, ignore.case = TRUE, recursive = TRUE)
  
  ## ignore anything in the rrt directory
  R_files <- grep("^rrt", R_files, invert = TRUE, value = TRUE)
  
  unlist(unique(sapply(R_files, deps_by_ext, dir=dir)))
}

# r <- "\\.[rR]$"
# rmd <- "\\.[rR]md$"
# rnw <- "\\.[rR]nw$"
# rpres <- "\\.[rR]pres$"
# txt <- "\\.txt$"
# extstrings <- c(r, rmd, rnw, rpres, txt)
# extstringsuse <- if(is.null(fileext)) extstrings else switch(fileext,
# )
# removeexcl <- function(x) if( grepl('^-', x) ) FALSE else TRUE
# sapply(fileext, removeexcl)

# ad-hoc dispatch based on the file extension
deps_by_ext <- function(file, dir) {
  file <- file.path(dir, file)
  fileext <- tolower(gsub(".*\\.", "", file))
  switch(fileext,
         r = deps.R(file),
         rmd = deps.Rmd(file),
         rnw = deps.Rnw(file),
         rpres = dep.Rpres(file),
         txt = deps.txt(file),
         #           md = deps.md(file),
         stop("Unrecognized file type '", file, "'")
  )
}

deps.Rmd <- dep.Rpres <- function(file) {
  if (require("knitr")) {
    tempfile <- tempfile()
    on.exit(unlink(tempfile))
    tryCatch(knitr::knit(file, output = tempfile, tangle = TRUE, quiet = TRUE), error = function(e) {
      mssg(verbose, "Unable to knit file '", file, "'; cannot parse dependencies")
      character()
    })
    deps.R(tempfile)
  } else {
    warning("knitr is required to parse dependencies from .Rmd files, but is not available")
    character()
  }
}

deps.Rnw <- function(file) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  tryCatch(Stangle(file, output = tempfile, quiet = TRUE), error = function(e) {
    mssg(verbose, "Unable to stangle file '", file, "'; cannot parse dependencies")
    character()
  })
  deps.R(tempfile)
}

deps.R <- deps.txt <- function(file) {
  
  if (!file.exists(file)) {
    warning("No file at path '", file, "'.")
    return(character())
  }
  
  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  tryCatch({
    exprs <- suppressWarnings(parse(file, n = -1L))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) {
    warning(paste("Failed to parse", file, "; dependencies in this file will",
                  "not be discovered."))
  })
  
  # return packages
  unique(pkgs)
}

# detect the package dependencies of an expression (adapted from
# tools:::.check_packages_used)
#
# expressionDependencies(quote(library("h")))
# expressionDependencies(quote(library(10, package = "h")))
# expressionDependencies(quote(library(h)))
# expressionDependencies(quote({library(h); library(g)}))
# expressionDependencies(quote(h::f))
expressionDependencies <- function(e) {
  # base case
  if (is.atomic(e) || is.name(e)) return()
  
  # recursive case: expression (= list of calls)
  if (is.expression(e)) {
    return(unlist(lapply(e, expressionDependencies)))
  }
  
  # base case: a call
  fname <- as.character(e[[1L]])
  # a refclass method call, so return
  if (length(fname) > 1) return()
  
  if (length(fname) == 1) {
    
    # base case: call to library/require
    if (fname %in% c("library", "require")) {
      mc <- match.call(get(fname, baseenv()), e)
      if (is.null(mc$package)) return(NULL)
      if (isTRUE(mc$character.only)) return(NULL)
      
      return(as.character(mc$package))
    }
    
    # base case: call to :: or :::
    if (fname %in% c("::", ":::")) (
      return(as.character(e[[2L]]))
    )
    
    # base case: methods functions
    if (fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric")) {
      return("methods")
    }
    
  }
  
  # recursive case: all other calls
  children <- lapply(as.list(e[-1]), expressionDependencies)
  unique(unlist(children))
}