withr::local_envvar(R_USER_CACHE_DIR=tempfile(), .local_envir=teardown_env())


list_pkgsrc <- function(snapshot_dir)
{
    pkgs <- dir(snapshot_dir, full.names=TRUE)
    srcs <- sapply(pkgs, function(pkg)
    {
        desc <- file.path(pkg, "DESCRIPTION")
        if(file.exists(desc))
            grep("^RemoteRepo", readLines(desc), value=TRUE)
        else ""
    })
    unname(srcs[srcs != ""])
}


list_pkgref <- function(snapshot_dir)
{
    pkgs <- dir(snapshot_dir, full.names=TRUE)
    srcs <- sapply(pkgs, function(pkg)
    {
        desc <- file.path(pkg, "DESCRIPTION")
        if(file.exists(desc))
            grep("^RemotePkgRef", readLines(desc), value=TRUE)
        else ""
    })
    unname(srcs[srcs != ""])
}

