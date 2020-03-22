install_pkgs <- function(pkgs, snapshot_date, libdir, mran_url, r_version)
{
    if(length(pkgs) == 0)
        return()
    print(pkgs)
    snapshot_url <- httr::parse_url(mran_url)
    snapshot_url$path <- file.path("snapshot", snapshot_date)

    config <- list(
        `cran-mirror`=httr::build_url(snapshot_url),
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
