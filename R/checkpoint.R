#' Configures R session to use packages as they existed on CRAN at time of snapshot.
#'
#' Together, the checkpoint package and the checkpoint server act as a CRAN time machine.  The `checkpoint()` function installs the packages referenced in the specified project to a local library exactly as they existed at the specified point in time.  Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results.  In this way, anyone using the checkpoint `checkpoint()` function can ensure the reproducibility of your scripts or projects at any time.
#'
#' @section Details:
#'
#' `create_checkpoint()` creates a local library into which it installs a copy of the packages required by your project as they existed on CRAN on the specified snapshot date.  Your R session is updated to use only these packages.
#'
#' To determine the packages used in your project, the function scans all R code (`.R`, `.Rmd`, `.Rnw`, `.Rhtml` and `.Rpres` files) for [library()] and [require()] statements. In addition, it scans for occurrences of code that accesses functions in namespaces using `package[::]foo()` and `package[:::]foo()`.
#'
#' Specifically, the function will:
#'
#' * Create a new local snapshot library to install packages.  By default this library folder is at `~/.checkpoint` but you can modify the path using the `checkpointLocation` argument.
#' * Update the options for your CRAN mirror and point to an MRAN snapshot using [options]`(repos)`
#' * Scan your project folder for all required packages and install them from the snapshot using [`pkgdepends::new_pkg_installation_proposal`]
#'
#' @section Resetting the checkpoint:
#'
#' To reset the checkpoint, call `uncheckpoint_session()`.
#'
#' @section Last accessed date:
#'
#' The [`create_checkpoint`] function stores a marker in the snapshot folder every time the function gets called. This marker contains the system date, thus indicating the the last time the snapshot was accessed.
#'
#' @param snapshot_date Date of snapshot to use in `YYYY-MM-DD` format, e.g. `"2014-09-17"`.  Specify a date on or after `"2014-09-17"`.  MRAN takes one snapshot per day. To list all valid snapshot dates on MRAN use [`list_mran_snapshots`].
#'
#' @param r_version Optional character string, e.g. `"3.6.2"`.  If specified, compares the current [`R.version`] to the specified version. If these differ, stops processing with an error, making no changes to the system.
#'
#' @param project_dir A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for `snapshotDate`. Defaults to the current working directory.
#'
#' @param checkpoint_location File path where the checkpoint library is stored.  Default is `"~"`, i.e. the user's home directory. A use case for changing this is to create a checkpoint library on a portable drive (e.g. USB drive), or to create per-project checkpoints. The actual checkpoints will be created under a `.checkpoint` directory at this location.
#'
#' @param mran_url The base MRAN URL. The default is taken from the system option `checkpoint.mranUrl`, or if this is unset, `https://mran.microsoft.com`. Currently checkpoint 1.0 does not support local MRAN mirrors.
#'
#' @param scan_now If `TRUE`, scans for packages in project folder (see details). If `FALSE`, skips the scanning process. Set this to `FALSE` if you only want to create the checkpoint subdirectory structure.
#'
#' @param use_now If `TRUE`, `create_checkpoint` calls `use_checkpoint` after the checkpoint has been created. Ignored if `scan_now=FALSE`.
#'
#' @param scan_r_only If `TRUE`, limits the scanning of project files to R scripts only (those with the extension ".R").
#'
#' @param scan_rnw_with_knitr If `TRUE`, scans Sweave files (those with extension ".Rnw") with [`knitr::knitr`], otherwise with [`utils::Stangle`]. Ignored if `scan_r_only=TRUE`.
#'
#' @param scan_rprofile if `TRUE`, includes the `~/.Rprofile` startup file in the scan. See [`Startup`].
#'
#' @param log If `TRUE`, writes logging information (mostly the output from the methods of [`pkgdepends::pkg_installation_proposal`]) to the checkpoint directory.
#'
#' @param num_workers The number of parallel workers to use for installing packages. Defaults to the minimum of 3 and the number of physical cores on the machine.
#'
#' @param config Further configuration parameters to pass to [`pkgdepends::new_pkg_installation_proposal`].
#'
#' @param ... Further object initialisation arguments to pass to [`pkgdepends::pkg_installation_proposal`].
#'
#' @param prepend If `TRUE`, adds the checkpoint directory to the beginning of [`.libPaths`]. The default is `FALSE`, where the checkpoint directory replaces all but the last entry in `.libPaths`; this is to reduce the chances of accidentally calling non-checkpointed code. Note that the packages shipped with R itself are never checkpointed, and always remain part of `.libPaths`.
#'
#' @return
#' Checkpoint is called for its side-effects (see the details section), but invisibly returns an object of class `pkgdepends::pkg_installation_proposal`.
#'
#' @rdname checkpoint
#' @export
#' @family checkpoint functions
#' @example /inst/examples/example_checkpoint.R
create_checkpoint <- function(snapshot_date,
                              r_version=getRversion(),
                              project_dir=".",
                              checkpoint_location="~",
                              mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                              scan_now=TRUE,
                              use_now=TRUE,
                              scan_r_only=FALSE,
                              scan_rnw_with_knitr=TRUE,
                              scan_rprofile=TRUE,
                              log=TRUE,
                              num_workers=NULL,
                              config=list(),
                              prepend=FALSE,
                              ...
                             )
{
    if(package_version(r_version) != getRversion())
        stop("R version does not match")

    # create checkpoint dir
    snapshot_date <- verify_date(snapshot_date)
    create_checkpoint_dir(snapshot_date, checkpoint_location, r_version)

    if(!scan_now)
        return(invisible(NULL))

    # scan files
    dep_list <- scan_project_files(project_dir, scan_r_only, scan_rnw_with_knitr, scan_rprofile)
    if(length(dep_list$pkgs) == 0)
    {
        warning("No package dependencies found", call.=FALSE)
        return(invisible(NULL))
    }

    # install packages
    inst <- install_pkgs(dep_list$pkgs, snapshot_date, checkpoint_location, mran_url, r_version, log, num_workers,
                         config, ...)

    # set .libPaths/repos
    if(use_now)
        use_checkpoint(snapshot_date, r_version, checkpoint_location, prepend=prepend)
    else set_access_date(snapshot_date, checkpoint_location)

    invisible(inst)
}

#' @rdname checkpoint
#' @export
use_checkpoint <- function(snapshot_date,
                           r_version=getRversion(),
                           checkpoint_location="~",
                           mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                           prepend=FALSE
                          )
{
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)
    if(!dir.exists(libdir))
        stop("Checkpoint directory not found", call.=FALSE)

    set_access_date(snapshot_date, checkpoint_location)

    # replace all unnamed repos and CRAN-repos
    repos <- getOption("repos")
    repo_names <- names(repos)
    if(is.null(repo_names))
        options(repos=c(CRAN=snapshot_url(snapshot_date, mran_url)))
    else
    {
        unnamed_or_cran <- repo_names %in% c("", "CRAN")
        repos <- c(CRAN=snapshot_url(snapshot_date, mran_url), repos[!unnamed_or_cran])
        options(repos=repos)
    }

    if(prepend)
        .libPaths(c(libdir, .libPaths()))
    else .libPaths(c(libdir, .libPaths()[length(.libPaths())]))  # .Library/.libPaths()[n] may be a symlink
    invisible(NULL)
}

#' @rdname checkpoint
#' @export
delete_checkpoint <- function(snapshot_date, r_version=getRversion(), checkpoint_location="~")
{
    # stop if checkpoint in use
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)
    if(libdir %in% .libPaths())
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(libdir, recursive=TRUE)
}

#' @rdname checkpoint
#' @export
delete_all_checkpoints <- function(checkpoint_location="~")
{
    checkpoint_root <- normalizePath(file.path(checkpoint_location, ".checkpoint"), winslash="/", mustWork=FALSE)
    if(any(grepl(checkpoint_root, .libPaths(), fixed=TRUE)))
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(checkpoint_root, recursive=TRUE)
}

#' @rdname checkpoint
#' @export
uncheckpoint_session <- function()
{
    options(repos=.checkpoint$old_repos)
    .libPaths(.checkpoint$old_libpath)
    invisible(NULL)
}


verify_date <- function(date)
{
    realdate <- try(as.Date(date), silent=TRUE)
    if(inherits(realdate, "try-error"))
        stop("Invalid date, must be in the format YYYY-MM-DD", call.=FALSE)
    if(realdate < as.Date("2014-09-17"))
        stop("Snapshots are only available after 2014-09-17", call.=FALSE)
    if(realdate > Sys.Date())
        stop("Snapshot date later than current date", call.=FALSE)
    date
}


