.checkpoint <- new.env()

.onLoad <- function(libname, pkgname)
{
    .checkpoint$old_libpath <- .libPaths()
    .checkpoint$old_repos <- getOption("repos")
}

.onAttach <- function(libname, pkgname)
{
    msg <- paste(
        "",
        "checkpoint: Part of the Reproducible R Toolkit from Microsoft",
        "https://mran.microsoft.com/documents/rro/reproducibility/",
        sep="\n")
    packageStartupMessage(msg)
}

