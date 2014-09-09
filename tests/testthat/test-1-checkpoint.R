# tests for initialize
context("checkpoint")

repo_root <- file.path(tempdir(), "rrttemp")
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder(repo_root)



test_that("snapshot functions return correct results", {
  
  snap_date <- "2014-09-08"
  
  expect_equal(
    getSnapshotUrl(snap_date),
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
  
  x <- repoInstalledPackages(repo_root)
  expect_equal(
    x[, "Package"], 
    c("bitops", "digest", "httr", "jsonlite", "MASS", "plyr", "Rcpp", 
      "RcppTestA", "testRcppClass", "testRcppModule", "RCurl", "stringr", "XML")
  )
  
  expect_true(
    all(na.omit(x[, "Date/Publication"]) <= as.POSIXct(snap_date, tz="UTC"))
    )
  
  
  expect_equal(
    getOption("repos"),
    "http://cran-snapshots.revolutionanalytics.com/2014-09-08_1746"
  )
  
  
  expect_equal(
    rrtPath(repo_root, "lib"),
    normalizePath(.libPaths()[1])
  )
  
})




# cleanup
cleanRRTfolder(repo_root)


