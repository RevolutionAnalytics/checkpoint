list_snapshot_dates <- function(mran_url=getOption("checkpoint.mranUrl", "https://mran.microsoft.com"))
{
    snapshot_url <- httr::parse_url(mran_url)
    snapshot_url$path <- file.path(snapshot_url$path, "snapshot")

    lines <- if(grepl("^http", snapshot_url$scheme))
        readLines(httr::build_url(snapshot_url))
    else if(snapshot_url$scheme == "file")
    {
        f <- file(httr::build_url(snapshot_url))
        on.exit(close(f))
        dir(summary(f))
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

