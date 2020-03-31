install_pkgs <- function(pkgs, snapshot_date, checkpoint_location, mran_url, r_version, log, num_workers, config, ...)
{
    if(length(pkgs) == 0)
        return()

    config <- utils::modifyList(list(
        `cran-mirror`=snapshot_url(mran_url, snapshot_date),
        library=checkpoint_dir(snapshot_date, checkpoint_location, r_version),
        `r-versions`=as.character(r_version)
    ), config)

    logtime <- Sys.time()

    withr::with_options(list(Ncpus=num_workers, repos=NULL),
    {
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
        inst_res <- inst$install()
        warn_for_install_error(inst_res)
        write_checkpoint_log(inst_res, "install", checkpoint_location, logtime, log)
    })
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


warn_for_install_error <- function(install_result)
{
    is_empty <- function(x) length(x) == 0

    success <- sapply(install_result$error, is_empty) & sapply(install_result$download_error, is_empty)
    if(!all(success))
        warning("Some packages failed to install:\n ", paste(install_result$package[!success], sep=" "), call.=FALSE)
    NULL
}
