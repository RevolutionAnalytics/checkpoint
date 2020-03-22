# idempotent: creating multiple times == creating once
create_checkpoint <- function(snapshot_date,
                              project_dir=getwd(),
                              checkpoint_location="~",
                              mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                              r_version=NULL,
                              scan_r_only=FALSE,
                              scan_rnw_with_knitr=FALSE,
                              use_now=TRUE,
                              log=c("csv", "json", "none"),
                              config=list(),
                              ...
                             )
{
    if(is.null(r_version))
        r_version <- getRversion()
    else if(package_version(r_version) != getRversion())
        stop("R version does not match")

    # scan files
    dep_list <- scan_project_files(project_dir, scan_r_only, scan_rnw_with_knitr)
    if(length(dep_list$pkgs) == 0)
    {
        warning("No package dependencies found", call.=FALSE)
        return(invisible(NULL))
    }

    # create checkpoint dir
    snapshot_date <- verify_date(snapshot_date)
    libdir <- create_checkpoint_dir(snapshot_date, checkpoint_location, r_version)

    # install packages
    inst <- install_pkgs(dep_list$pkgs, snapshot_date, libdir, mran_url, r_version, config, ...)

    # log, if required
    log <- match.arg(log)
    write_checkpoint_log(inst, checkpoint_location, log)

    # set .libPaths()
    if(use_now)
        use_checkpoint(snapshot_date, checkpoint_location)
    else set_access_date(snapshot_date, checkpoint_location)

    invisible(inst)
}

.checkpoint <- new.env()

use_checkpoint <- function(snapshot_date,
                           checkpoint_location="~",
                           mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
                           r_version=getRversion()
                          )
{
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)
    if(!dir.exists(libdir))
        stop("Checkpoint directory not found", call.=FALSE)

    .checkpoint$old_libpath <- .libPaths()
    .checkpoint$old_repos <- getOption("repos")
    set_access_date(snapshot_date, checkpoint_location)
    mran_url <- snapshot_url(snapshot_date, mran_url)

    options(repos=c(CRAN=mran_url))
    invisible(.libPaths(c(libdir, .Library)))
}

delete_checkpoint <- function(snapshot_date, checkpoint_location="~", r_version=getRversion())
{
    # stop if checkpoint in use
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)
    if(libdir %in% .libPaths())
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(libdir, recursive=TRUE)
}

delete_all_checkpoints <- function(checkpoint_location="~")
{
    checkpoint_root <- normalizePath(file.path(checkpoint_location, ".checkpoint"),
        winslash="/", mustWork=FALSE)
    if(any(grepl(checkpoint_root, .libPaths(), fixed=TRUE)))
        stop("Cannot delete checkpoint while in use, call reset_libpaths() first", call.=FALSE)
    unlink(checkpoint_root, recursive=TRUE)
}

reset_libpaths <- function()
{
    if(is.null(.checkpoint$old_libpath))
        stop("Original library path not saved, cannot reset", call.=FALSE)
    if(!is.null(.checkpoint$old_repos))
        options(repos=c(CRAN=.checkpoint$old_repos))
    invisible(.libPaths(.checkpoint$old_libpath))
}


verify_date <- function(date)
{
    realdate <- try(as.Date(date), silent=TRUE)
    if(inherits(realdate, "try-error"))
        stop("Invalid date, must be in the format YYYY-MM-DD", call.=FALSE)
    if(realdate < as.Date("2014-09-17"))
        stop("Snapshots are only available after 2014-09-17", call.=FALSE)
    if(realdate > Sys.Date())
        stop("snapshotDate can not be in the future!", call.=FALSE)
    date
}


