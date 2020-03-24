# idempotent: creating multiple times == creating once
create_checkpoint <- function(snapshot_date,
                              r_version=NULL,
                              project_dir=getwd(),
                              checkpoint_location="~",
                              mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                              scan_now=TRUE,
                              use_now=TRUE,
                              scan_r_only=FALSE,
                              scan_rnw_with_knitr=FALSE,
                              log=TRUE,
                              num_workers=NULL,
                              config=list(),
                              prepend=FALSE,
                              ...
                             )
{
    if(is.null(r_version))
        r_version <- getRversion()
    else if(package_version(r_version) != getRversion())
        stop("R version does not match")

    # create checkpoint dir
    snapshot_date <- verify_date(snapshot_date)
    create_checkpoint_dir(snapshot_date, checkpoint_location, r_version)

    if(!scan_now)
        return(invisible(NULL))

    # scan files
    dep_list <- scan_project_files(project_dir, scan_r_only, scan_rnw_with_knitr)
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

delete_checkpoint <- function(snapshot_date, r_version=getRversion(), checkpoint_location="~")
{
    # stop if checkpoint in use
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)
    if(libdir %in% .libPaths())
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(libdir, recursive=TRUE)
}

delete_all_checkpoints <- function(checkpoint_location="~")
{
    checkpoint_root <- normalizePath(file.path(checkpoint_location, ".checkpoint"), winslash="/", mustWork=FALSE)
    if(any(grepl(checkpoint_root, .libPaths(), fixed=TRUE)))
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(checkpoint_root, recursive=TRUE)
}

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

    # message("Checking available snapshot dates... ", appendLF=FALSE)
    # valid_dates <- list_snapshot_dates(mran_url)
    # if(!(realdate %in% as.Date(valid_dates)))
    #     stop("Snapshot date ", date, " not found", call.=FALSE)
    # else message("OK")
    date
}


