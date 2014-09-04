#' Detect repository packages.
#'
#' Detect all packages used in repository. Parses all .R, .Rmd, .Rnw, .Rpres, and other files in the repository directory to determine what packages the repo depends directly.
#'
#' Only direct dependencies are detected (i.e. no recursion).
#'
#'
#' Packages are determined by parsing repository source code and looking for calls to \code{library}, \code{require}, \code{::}, and \code{:::}.
#' 
#' @export
#'
#' @param repo Repository path. Defaults to current working directory.
#' @param fileext (character) File extensions to search for. Default is .R, .Rmd, .Rnw, and .Rpres.  Specify a certain file extension(s) in a charcter vector. Exclude a file extension by e.g. "-.Rmd"
#' @param verbose (logical) Print messages or not.
#'
#' @return Vector of package names on which R code in the repository depends.
#'
#' @family rrt
#' 
#' @example /inst/examples/example_scanRepoPackages.R
#' @note Modified from dependencies.R in the packrat github repo

scanRepoPackages <- function(repo = getwd(), fileext = NULL, verbose = TRUE){
  repo <- normalizePath(repo, mustWork=FALSE)
  
  # detect all package dependencies for a repo
  dir <- normalizePath(repo, winslash='/')
  pattern <- "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$"
  R_files <- list.files(dir, pattern = pattern, ignore.case = TRUE, recursive = TRUE)
  
  ## ignore anything in the rrt directory
  R_files <- grep("^rrt", R_files, invert = TRUE, value = TRUE)
  
  pkgs <- unlist(unique(sapply(R_files, deps_by_ext, dir=dir)))
  as.vector(pkgs)
  
}



# ad-hoc dispatch based on the file extension
deps_by_ext <- function(file, dir) {
  file <- file.path(dir, file)
  fileext <- tolower(gsub(".*\\.", "", file))
  switch(fileext,
         r = deps.R(file),
         rmd = deps.Rmd(file),
         rnw = deps.Rnw(file),
         rpres = deps.Rpres(file),
         txt = deps.txt(file),
         stop("Unrecognized file type '", file, "'")
  )
}

#' @import knitr
deps.Rmd <- deps.Rpres <- function(file, verbose=TRUE) {
    tempfile <- tempfile()
    on.exit(unlink(tempfile))
    tryCatch(knitr::knit(file, output = tempfile, tangle = TRUE, quiet = TRUE), error = function(e) {
      mssg(verbose, "Unable to knit file '", file, "'; cannot parse dependencies")
      character()
    })
    deps.R(tempfile)
}

deps.Rnw <- function(file, verbose=TRUE) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  tryCatch(Stangle(file, output = tempfile, quiet = TRUE), error = function(e) {
    mssg(verbose, "Unable to stangle file '", file, "'; cannot parse dependencies")
    character()
  })
  deps.R(tempfile)
}

deps.R <- deps.txt <- function(file, verbose=TRUE) {

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
