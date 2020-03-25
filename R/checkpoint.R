#' Configures R session to use packages as they existed on CRAN at time of snapshot.
#'
#' Together, the checkpoint package and the checkpoint server act as a CRAN time machine. The `create_checkpoint` function installs the packages referenced in the specified project to a local library exactly as they existed at the specified point in time.  Only those packages are available to your session, thereby avoiding any package updates that came later and may have altered your results. In this way, anyone using the `use_checkpoint` function can ensure the reproducibility of your scripts or projects at any time.
#'
#' The `checkpoint` function serves as a simple umbrella interface to these functions. It first tests if the checkpoint exists, creates if necessary with `create_checkpoint`, and then calls `use_checkpoint`.
#'
#' @section Details:
#'
#' `create_checkpoint` creates a local library (by default, located under your home directory) into which it installs copies of the packages required by your project as they existed on CRAN on the specified snapshot date. To determine the packages used in your project, the function scans all R code (`.R`, `.Rmd`, `.Rnw`, `.Rhtml` and `.Rpres` files) for [`library`] and [`require`] statements, as well as the namespacing operators `::` and `:::`.
#'
#' Checkpoint only installs packages that can be found on CRAN. This includes third-party packages, as well as those distributed as part of R that have the "Recommends" priority. Base-priority packages (the workhorse engine of R, including utils, graphics, methods and so forth) are not checkpointed (but see the `r_version` argument above). A future update may add the ability to install non-CRAN packages, such as those found on BioConductor or GitHub.
#'
#' The package installation is carried out via the [pkgdepends] package, which has many features including cached installs, parallel installs, and comprehensive reporting of outcomes. It also solves many problems that previous versions of checkpoint struggled with, such as being able to install packages that are in use, and reliably detecting the outcome of the installation process.
#'
#' `checkpoint` is a convenience function that calls `create_checkpoint` if the checkpoint directory does not exist, and then `use_checkpoint`.
#'
#' `use_checkpoint` modifies your R session to use only the packages installed by `create_checkpoint`. Specifically, it changes your library search path via `.libPaths()` to point to the checkpointed library, and updates the options for your CRAN mirror to point to an MRAN snapshot using `options(repos)`.
#'
#' `delete_checkpoint` deletes a checkpoint, after ensuring that it is no longer in use. `delete_all_checkpoints` deletes _all_ checkpoints under the given checkpoint location.
#'
#' `uncheckpoint_session` is the reverse of `use_checkpoint`. It restores your library search path and CRAN mirror option to their original values, as they were before checkpoint was loaded. Call this before calling `delete_checkpoint` and `delete_all_checkpoints`.
#'
#' @section Last accessed date:
#'
#' The `create_checkpoint` and `use_checkpoint` functions store a marker in the snapshot folder every time the function gets called. This marker contains the system date, thus indicating the the last time the snapshot was accessed.
#'
#' @param snapshot_date Date of snapshot to use in `YYYY-MM-DD` format, e.g. `"2020-01-01"`.  Specify a date on or after `"2014-09-17"`.  MRAN takes one snapshot per day. To list all valid snapshot dates on MRAN, use [`list_mran_snapshots`].
#'
#' @param r_version Optional character string, e.g. `"3.6.2"`. If specified, compares the current [`R.version`] to the specified version. If these differ, `create_checkpoint` and `use_checkpoint` will throw an error without making any changes. The benefit of supplying this argument is that checkpoint can alert you when your R version changes while you are working on a project; this can just as easily lead to reproducibility issues as changes in third-party code. It's recommended that you supply an explicit value for this argument, although checkpoint will still function without it.
#'
#' @param checkpoint_location File path where the checkpoint library is stored.  Default is `"~"`, i.e. your home directory. Use cases for changing this include creating a checkpoint library on a portable drive (e.g. USB drive), or creating per-project checkpoints. The actual checkpoints will be created under a `.checkpoint` directory at this location.
#'
#' @param project_dir A project path.  This is the path to the root of the project that references the packages to be installed from the MRAN snapshot for the date specified for `snapshotDate`. Defaults to the current working directory.
#'
#' @param mran_url The base MRAN URL. The default is taken from the system option `checkpoint.mranUrl`, or if this is unset, `https://mran.microsoft.com`. Currently checkpoint 1.0 does not support local MRAN mirrors.
#'
#' @param scan_now If `TRUE`, scans for packages in the project folder (see 'Details'). If `FALSE`, skips the scanning process. Set this to `FALSE` if you only want to create the checkpoint subdirectory structure.
#'
#' @param scan_r_only If `TRUE`, limits the scanning of project files to R scripts only (those with the extension ".R").
#'
#' @param scan_rnw_with_knitr If `TRUE`, scans Sweave files (those with extension ".Rnw") with [`knitr::knitr`], otherwise with [`utils::Stangle`]. Ignored if `scan_r_only=TRUE`.
#'
#' @param scan_rprofile if `TRUE`, includes the `~/.Rprofile` startup file in the scan. See [Startup].
#'
#' @param log If `TRUE`, writes logging information (mostly the output from the methods of [`pkgdepends::pkg_installation_proposal`]) to the checkpoint directory.
#'
#' @param num_workers The number of parallel workers to use for installing packages. Defaults to the minimum of 3 and the number of physical cores on the machine.
#'
#' @param config Further configuration parameters to pass to [`pkgdepends::new_pkg_installation_proposal`]; possible parameters are listed at [`pkgdepends::pkg_config`]. Note that `create_checkpoint` will automatically pass the cran-mirror, library and r-version parameters, which are taken from the other arguments above.
#'
#' @param ... For `checkpoint`, further arguments to pass to `create_checkpoint` and `use_checkpoint`. Ignored for `create_checkpoint` and `use_checkpoint`.
#'
#' @param prepend If `TRUE`, adds the checkpoint directory to the beginning of [`.libPaths`]. The default is `FALSE`, where the checkpoint directory replaces all but the last entry in `.libPaths`; this is to reduce the chances of accidentally calling non-checkpointed code. Note that the R library directory itself always remains part of `.libPaths`.
#'
#' @return
#' These functions are run mostly for their side-effects; however `create_checkpoint` invisibly returns an object of class `pkgdepends::pkg_installation_proposal` if `scan_now=TRUE`, and `NULL` otherwise. `checkpoint` returns the result of `create_checkpoint` if the checkpoint had to be created, otherwise `NULL`.
#'
#' @rdname checkpoint
#' @export
#' @family checkpoint functions
#' @example /inst/examples/example_checkpoint.R
checkpoint <- function(snapshot_date, r_version=getRversion(), checkpoint_location="~", ...)
{
    inst <- NULL
    if(!dir.exists(checkpoint_dir(snapshot_date, checkpoint_location, r_version)))
        inst <- create_checkpoint(snapshot_date, r_version, checkpoint_location, ...)

    use_checkpoint(snapshot_date, r_version, checkpoint_location, ...)
    invisible(inst)
}

#' @rdname checkpoint
#' @export
create_checkpoint <- function(snapshot_date,
                              r_version=getRversion(),
                              checkpoint_location="~",
                              project_dir=".",
                              mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                              scan_now=TRUE,
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
                         config)

    set_access_date(snapshot_date, checkpoint_location)
    invisible(inst)
}

#' @rdname checkpoint
#' @export
use_checkpoint <- function(snapshot_date,
                           r_version=getRversion(),
                           checkpoint_location="~",
                           mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                           prepend=FALSE,
                           ...
                          )
{
    if(package_version(r_version) != getRversion())
        stop("R version does not match")

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
        stop("Cannot delete checkpoint while in use, call uncheckpoint_session() first", call.=FALSE)
    unlink(libdir, recursive=TRUE)
}

#' @rdname checkpoint
#' @export
delete_all_checkpoints <- function(checkpoint_location="~")
{
    checkpoint_root <- normalizePath(file.path(checkpoint_location, ".checkpoint"), winslash="/", mustWork=FALSE)
    if(any(grepl(checkpoint_root, .libPaths(), fixed=TRUE)))
        stop("Cannot delete checkpoint while in use, call uncheckpoint_session() first", call.=FALSE)
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


