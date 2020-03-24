#' Scan R files for package dependencies
#'
#' @param project_dir A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for `snapshotDate`.  Defaults to current working directory using [`getwd`].
#'
#' @param scan_r_only If `TRUE`, limits the scanning of project files to R scripts only (those with the extension ".R").
#'
#' @param scan_rnw_with_knitr If `TRUE`, scans Sweave files (those with extension ".Rnw") with [`knitr::knitr`], otherwise with [`utils::Stangle`].
#'
#' @return
#' A list with 2 components: `pkgs`, a vector of package names, and `errors`, a vector of files that could not be scanned.
#' @export
scan_project_files <- function(project_dir=".", scan_r_only=FALSE, scan_rnw_with_knitr=TRUE)
{
    if(!scan_r_only)
    {
        res <- try(requireNamespace("knitr", quietly=TRUE))
        if(inherits(res, "try-error"))
            warning("The knitr package must be installed to scan Rmarkdown-based files", call.=FALSE)
    }

    r_pat <- if(scan_r_only)
        "\\.r$"
    else "\\.(r|rnw|rmd|rpres|rhtml)$"

    r_files <- dir(project_dir, pattern=r_pat, recursive=TRUE, ignore.case=TRUE)
    if(file.exists("~/.Rprofile"))
        r_files <- c(r_files, "~/.Rprofile")
    exclude <- c(
        # this package
        "checkpoint",
        # while developing
        "pkgdepends",
        # all base priority packages, not on CRAN or MRAN
        c("base", "compiler", "datasets", "graphics", "grDevices", "grid",
          "methods", "parallel", "splines", "stats", "stats4", "tcltk",
          "tools", "utils")
    )
    pkgs <- character(0)
    errors <- character(0)

    for(f in r_files)
    {
        scan_deps <- switch(tolower(tools::file_ext(f)),
            "r"=scan_r,
            "rmd"=, "rpres"=, "rhtml"=scan_rmd,
            "rnw"=if(scan_rnw_with_knitr) scan_rmd else scan_rnw,
            scan_r
        )
        res <- try(scan_deps(f), silent=TRUE)
        if(inherits(res, "try-error"))
            errors <- c(errors, f)
        else pkgs <- c(pkgs, res)
    }
    if(length(errors) > 0)
        warning("Following files could not be scanned:\n", paste(errors, collapse="\n"), call.=FALSE)

    pkgs <- setdiff(unique(pkgs), exclude)
    list(pkgs=pkgs, errors=errors)
}

scan_rmd <- function(filename)
{
    tempfile <- tempfile(fileext=".R")
    on.exit(unlink(tempfile))
    suppressWarnings(knitr::knit(filename, output=tempfile, tangle=TRUE, quiet=TRUE))
    scan_r(tempfile)
}

scan_rnw <- function(filename)
{
    tempfile <- tempfile(fileext=".R")
    on.exit(unlink(tempfile))
    suppressWarnings(utils::Stangle(filename, output=tempfile, quiet=TRUE))
    scan_r(tempfile)
}

scan_r <- function(filename)
{
    exprs <- parse(filename)
    deps <- character(0)
    for(e in exprs)
    {
        dep_e <- find_dependencies(e)
        if(length(dep_e) > 0)
        deps <- c(deps, dep_e)
    }
    unique(deps)
}

find_dependencies <- function(e)
{
    if(is.atomic(e) || is.name(e))
        return(character(0))

    if(is.call(e))
    {
        fname <- as.character(e[[1]])

        if(length(fname) == 1)
        {
            if(fname %in% c("library", "require"))
            {
                mc <- match.call(get(fname, baseenv()), e)
                return(as.character(mc$package))
            }
            else if(fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric"))
                return("methods")
            else return(unique(unlist(lapply(as.list(e[-1]), find_dependencies))))
        }
        else if(fname[1] %in% c("::", ":::"))
            return(fname[2])
        else return(character(0))
    }
}
