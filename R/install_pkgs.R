install_pkgs <- function(pkgs, snapshot_date, libdir, mran_url, r_version)
{
    if(length(pkgs) == 0)
        return()

    config <- list(
        `cran-mirror`=snapshot_url(snapshot_date, mran_url),
        library=libdir,
        `r-versions`=as.character(r_version)
    )

    inst <- pkgdepends::new_pkg_installation_proposal(pkgs, config=config)
    inst$resolve()
    inst$solve()
    inst$download()
    inst$install()

    inst
}


snapshot_url <- function(snapshot_date, mran_url)
{
    snapshot_url <- httr::parse_url(mran_url)
    snapshot_url$path <- file.path("snapshot", snapshot_date)
    httr::build_url(snapshot_url)
}
