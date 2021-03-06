% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mran_utils.R
\name{use_mran_snapshot}
\alias{use_mran_snapshot}
\alias{list_mran_snapshots}
\title{Utilities for working with MRAN snapshots}
\usage{
use_mran_snapshot(
  snapshot_date,
  mran_url = getOption("checkpoint.mranUrl", "https://mran.microsoft.com"),
  validate = FALSE
)

list_mran_snapshots(
  mran_url = getOption("checkpoint.mranUrl", "https://mran.microsoft.com")
)
}
\arguments{
\item{snapshot_date}{Date of snapshot to use in \code{YYYY-MM-DD} format, e.g. \code{"2020-01-01"}. Specify a date on or after \code{"2014-09-17"}.}

\item{mran_url}{The base MRAN URL. The default is taken from the system option \code{checkpoint.mranUrl}, or if this is unset, \verb{https://mran.microsoft.com}.}

\item{validate}{For \code{use_mran_snapshot}, whether to check if the snapshot date is valid (exists on the server).}
}
\value{
For \code{use_mran_snapshot}, the new value of \code{getOption("repos")}, invisibly. For \code{list_mran_snapshots}, a character vector of snapshot dates.
}
\description{
These functions are for working with the MRAN checkpoint server. \code{use_mran_snapshot} updates the CRAN mirror for your R session to point to an MRAN snapshot, using \code{options(repos)}. \code{list_mran_snapshots} returns the dates for which an MRAN snapshot exists.
}
\examples{
\dontrun{

list_mran_snapshots()

use_mran_snapshot("2020-01-01")

# validate=TRUE will detect an invalid snapshot date
use_mran_snapshot("1970-01-01", validate=TRUE)

}
}
