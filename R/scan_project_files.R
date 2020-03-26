#' Scan R files for package dependencies
#'
#' This function scans the R files in your project, including scripts, Sweave documents and Rmarkdown-based files, for references to packages.
#'
#' @param project_dir A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for `snapshotDate`. Defaults to the current working directory.
#'
#' @param scan_r_only If `TRUE`, limits the scanning of project files to R scripts only (those with the extension `.R`).
#'
#' @param scan_rnw_with_knitr If `TRUE`, scans Sweave files with [`knitr::knitr`], otherwise with [`utils::Stangle`]. Ignored if `scan_r_only=TRUE`.
#'
#' @param scan_rprofile if `TRUE`, includes the `~/.Rprofile` startup file in the scan. See [`Startup`].
#'
#' @details
#' `scan_project_files` recursively builds a list of all the R files in your project. This includes regular R scripts, as well as Sweave files (those with extension `.Rnw`) and Rmarkdown-based files (those with extension `.Rmd`, `.Rpres` or `Rhtml`). It then parses the code in each file and looks for calls to `library` and `require`, as well as the namespacing operators `::` and `:::`.
#'
#' @section Manifest:
#'
#' As an **experimental feature**, you can specify additional packages to include or exclude via an optional `checkpoint.yml` manifest file located in your project directory. This should be a valid YAML file with 2 components:
#' - `refs`: An array of package references, that can be parsed by [`pkgdepends::new_pkg_installation_proposal`]. See [`pkgdepends::pkg_refs`] for a description of the reference syntax.
#' - `exclude`: An array of package names (without decorations) that should be excluded from the final list of dependencies.
#'
#' A manifest file allows you to include packages from BioConductor or GitHub in the checkpoint. You should use this feature with caution, as checkpoint does not check the versions of these packages. While pkgdepends allows you to specify a package version as part of a reference, it currently ignores the version during the download and installation process.
#'
#' A use case for exclusions might be if your workflow requires packages not on CRAN or other public repositories. For example, Microsoft Machine Learning Server (MMLS) comes with a number of proprietary packages for big data and in-database analytics. You can exclude these packages from checkpointing by listing them in the `exclude` list in the manifest. In this case, you must ensure that your packages are still visible to the checkpointed session, for example by setting the `prepend` argument to `use_checkpoint` to `TRUE`. If you share your project with collaborators, they will also need to have these packages installed on their machines beforehand.
#' @return
#' A list with 2 components: `pkgs`, a vector of package names, and `errors`, a vector of files that could not be scanned. The package listing includes third-party packages, as well as those that are distributed with R and have "Recommended" priority. Base-priority packages (utils, graphics, methods and so forth) are not included.
#'
#' In addition, if any Rmarkdown files are found, the package listing will include rmarkdown. This allows you to continue rendering them in a checkpointed session.
#' @examples
#'
#' scan_project_files()
#' @export
scan_project_files <- function(project_dir=".", scan_r_only=FALSE, scan_rnw_with_knitr=TRUE, scan_rprofile=TRUE)
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

    r_files <- dir(project_dir, pattern=r_pat, recursive=TRUE, ignore.case=TRUE, full.names=TRUE)
    if(scan_rprofile && file.exists("~/.Rprofile"))
        r_files <- c(r_files, "~/.Rprofile")

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

    mft <- read_manifest(project_dir)
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
    any_rmd <- any(grepl("\\.(rmd|rpres|rhtml)$", r_files, ignore.case=TRUE))

    pkgs <- setdiff(unique(c(pkgs, mft$refs, if(any_rmd) "rmarkdown")), c(mft$exclude, exclude))
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
            else return(unique(unlist(lapply(as.list(e[-1]), find_dependencies))))
        }
        else if(fname[1] %in% c("::", ":::"))
            return(fname[2])
        else return(character(0))
    }
}

read_manifest <- function(project_dir)
{
    mft_file <- file.path(project_dir, "checkpoint.yml")
    if(!file.exists(mft_file))
        mft_file <- file.path(project_dir, "checkpoint.yaml")
    if(!file.exists(mft_file))
        return(NULL)

    mft <- yaml::read_yaml(mft_file)
    mft[c("refs", "exclude")]
}

