
projectScanPackages <- function(project = getwd(), verbose = TRUE, use.knitr = FALSE){
  # detect all package dependencies for a project
  dir <- normalizePath(project, winslash='/', mustWork=FALSE)
  pattern <- if(!use.knitr) "\\.[rR]$" else
    "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$"
  
  R_files <- list.files(dir, pattern = pattern, ignore.case = TRUE, recursive = TRUE)
  
  pkgs <- sapplyProgressBar(R_files, deps_by_ext, dir=dir)
  sort(unique(unlist(pkgs)))
  
}

sapplyProgressBar <- function(X, FUN, ...){
  env <- environment()
  N <- length(X)
  counter <- 0
  pb <- txtProgressBar(min = 0, max = N, style = 3)
  on.exit(close(pb))
  
  wrapper <- function(...){
    curVal <- get("counter", envir = env)
    assign("counter", curVal + 1, envir = env)
    setTxtProgressBar(get("pb", envir = env), curVal + 1)
    FUN(...)
  }
  sapply(X, wrapper, ...)
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

deps.Rmd <- deps.Rpres <- function(file, verbose=TRUE) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  stopifnot(require("knitr"))
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
