checkpoint_dir <- function(snapshot_date, checkpoint_location, r_version)
{
    path <- file.path(checkpoint_location, ".checkpoint", snapshot_date, "lib", R.version$platform, r_version)
    normalizePath(path, winslash="/", mustWork=FALSE)
}

create_checkpoint_dir <- function(snapshot_date, checkpoint_location, r_version)
{
    libdir <- checkpoint_dir(snapshot_date, checkpoint_location, r_version)

    if(!dir.exists(libdir))
        dir.create(libdir, recursive=TRUE)
    if(!dir.exists(libdir))
        stop("Unable to create checkpoint directory", call.=FALSE)

    normalizePath(libdir, winslash="/", mustWork=FALSE)
}

set_access_date <- function(snapshot_date, checkpoint_location)
{
    marker_loc <- file.path(checkpoint_location, ".checkpoint", snapshot_date, ".lastaccessed")
    writeLines(snapshot_date, marker_loc)
    invisible(NULL)
}
