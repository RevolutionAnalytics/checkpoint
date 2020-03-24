#' Lists the snapshot dates found on MRAN
#'
#' @param mran_url The base MRAN URL. The default is taken from the system option `checkpoint.mranUrl`, or if this is unset, `https://mran.microsoft.com`.
#' @return
#' A character vector of snapshot dates.
#' @export
list_mran_snapshots <- function(mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"))
{
    snapshot_url <- httr::parse_url(mran_url)
    snapshot_url$path <- file.path(snapshot_url$path, "snapshot")

    if(grepl("^http", snapshot_url$scheme))
    {
        lines <- try(readLines(httr::build_url(snapshot_url)), silent=TRUE)
        if(inherits(lines, "try-error"))
            stop("Unable to contact MRAN host", call.=FALSE)
    }
    else if(snapshot_url$scheme == "file")
    {
        f <- file(httr::build_url(snapshot_url))
        on.exit(close(f))
        lines <- dir(summary(f))
    }
    else stop("Invalid URL scheme", call.=FALSE)

    date_pat <- "\\d{4}-\\d{2}-\\d{2}"
    lines <- grep(date_pat, lines, value=TRUE)
    gsub(sprintf("^<a href=.*?>(%s).*?</a>.*$", date_pat), "\\1", lines)
}


snapshot_url <- function(snapshot_date, mran_url)
{
    snapshot_url <- httr::parse_url(mran_url)
    snapshot_url$path <- file.path(snapshot_url$path, "snapshot", snapshot_date)
    httr::build_url(snapshot_url)
}

