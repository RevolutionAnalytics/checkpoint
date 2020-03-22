install_pkgs <- function(pkgs, snapshot_date, libdir, mran_url, r_version, config, ...)
{
    if(length(pkgs) == 0)
        return()

    config <- utils::modifyList(list(
        `cran-mirror`=snapshot_url(snapshot_date, mran_url),
        library=libdir,
        `r-versions`=as.character(r_version)
    ), config)

    inst <- pkgdepends::new_pkg_installation_proposal(pkgs, config=config, ...)
    inst$resolve()
    inst$solve()
    inst$stop_for_solution_error()
    inst$download()
    inst$stop_for_download_error()
    inst$install()

    inst
}


write_checkpoint_log <- function(pkg_install, checkpoint_location, log_format)
{
    if(log_format == "none")
        return(character(0))

    inst_plan <- pkg_install$get_install_plan()
    if(log_format == "csv")
    {
        logname <- file.path(checkpoint_location, ".checkpoint", "checkpoint_log.csv")
        keep <- sapply(inst_plan, function(x) inherits(x, c("character", "integer", "logical", "numeric")))
        write.csv(inst_plan[keep], logname, row.names=FALSE)
    }
    else
    {
        logname <- file.path(checkpoint_location, ".checkpoint", "checkpoint_log.json")
        writeLines(jsonlite::toJSON(inst_plan, auto_unbox=TRUE, pretty=TRUE, null="null"), logname)
    }
    invisible(logname)
}
