install_pkgs <- function(pkgs, snapshot_date, checkpoint_location, mran_url, r_version, log, num_workers, config, ...)
{
    if(length(pkgs) == 0)
        return()

    config <- utils::modifyList(list(
        `cran-mirror`=snapshot_url(snapshot_date, mran_url),
        library=checkpoint_dir(snapshot_date, checkpoint_location, r_version),
        `r-versions`=as.character(r_version)
    ), config)

    if(is.null(num_workers))
        num_workers <- min(parallel::detectCores(logical=FALSE), 3)

    old_ncpus <- options(Ncpus=num_workers)
    on.exit(if(!is.null(old_ncpus)) options(Ncpus=old_ncpus))
    logtime <- Sys.time()

    # workaround caching confusion with multiple package versions
    pkgcache::pkg_cache_delete_files()

    inst <- pkgdepends::new_pkg_installation_proposal(pkgs, config=config, ...)
    write_checkpoint_log(inst$get_config(), "config", checkpoint_location, logtime, log)

    inst$resolve()
    write_checkpoint_log(inst$get_resolution(), "resolution", checkpoint_location, logtime, log)

    inst$solve()
    write_checkpoint_log(inst$get_solution(), "solution", checkpoint_location, logtime, log)
    inst$stop_for_solution_error()

    inst$download()
    write_checkpoint_log(inst$get_downloads(), "downloads", checkpoint_location, logtime, log)
    inst$stop_for_download_error()

    write_checkpoint_log(inst$get_install_plan(), "install_plan", checkpoint_location, logtime, log)
    write_checkpoint_log(inst$install(), "install", checkpoint_location, logtime, log)

    inst
}


write_checkpoint_log <- function(object, name, checkpoint_location, logtime, do_logging)
{
    if(!do_logging)
        return(invisible(NULL))

    logtime <- strftime(logtime, "%Y%m%d_%H%M%S")
    name <- paste0(logtime, "_", name, ".json")
    pathname <- file.path(checkpoint_location, ".checkpoint", name)
    writeLines(jsonlite::toJSON(object, auto_unbox=TRUE, pretty=TRUE, null="null", force=TRUE), pathname)
    invisible(object)
}
