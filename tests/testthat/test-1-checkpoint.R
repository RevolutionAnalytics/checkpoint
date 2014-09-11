# tests for initialize
context("checkpoint")

repo_root <- file.path(tempdir(), "rrttemp")
dir.create(repo_root)
snap_date <- "2014-09-08"

test_that("snapshot functions return correct results", {

  RRT:::cleanRRTfolder(snap_date)

  expect_equal(
    RRT:::getSnapshotUrl(snap_date),
    "http://cran-snapshots.revolutionanalytics.com/2014-09-08"
  )

  expect_message(
    checkpoint(snap_date, repo = repo_root),
    "No packages found to install"
  )

  # Write dummy code file to repo
  cat("library(MASS)", "library(plyr)", "library(XML)", "library('httr')",
      sep="\n",
      file = file.path(repo_root, "code.R")
  )

  expect_message(
    checkpoint(snap_date, repo = repo_root),
    "Installing packages used in this repository"
  )

  x <- installed.packages(fields = "Date/Publication")
  expect_equivalent(
    sort(x[, "Package"]),
    sort(c("bitops", "digest", "httr", "jsonlite", "MASS", "plyr", "Rcpp",
           "RCurl", "stringr", "XML"))
  )

  expect_true(
    all(
      na.omit(
        x[, "Date/Publication"]) <=
        as.POSIXct(snap_date, tz="UTC"))
  )


  expect_equal(
    getOption("repos"),
    "http://cran-snapshots.revolutionanalytics.com/2014-09-08"
  )


  expect_equal(
    RRT:::rrtPath(snap_date, "lib"),
    normalizePath(.libPaths()[1])
  )

})




# cleanup
RRT:::cleanRRTfolder(snap_date)


