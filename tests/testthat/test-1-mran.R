# tests for initialize
context("mran")

rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))

exp <- structure(list(package = "plyr", description = structure(list(
  Package = "plyr", Version = "1.8.1", Depends = "R (>= 2.11.0)", 
  Imports = "Rcpp (>= 0.11.0)", LinkingTo = "Rcpp", Suggests = "abind, testthat, tcltk, foreach, doMC, itertools, iterators", 
  License = "MIT + file LICENSE", NeedsCompilation = "yes"), .Names = c("Package", 
                                                                        "Version", "Depends", "Imports", "LinkingTo", "Suggests", "License", 
                                                                        "NeedsCompilation")), snapshotId = "2014-08-04_0500", snapshotDate = "2014-08-04_0500", 
  snapshotDiffId = "2014-08-04_0500.txt", compatibitlityCheck = structure(list(), .Names = character(0)), 
  message = structure(list(), .Names = character(0)), source = structure(list(
    baseurl = "http://mran.revolutionanalytics.com/snapshots/src/2014-08-04_0500/plyr/", 
    ver = structure(list(`0.1.1` = "plyr_0.1.1.tar.gz", `0.1.2` = "plyr_0.1.2.tar.gz", 
                         `0.1.3` = "plyr_0.1.3.tar.gz", `0.1.4` = "plyr_0.1.4.tar.gz", 
                         `0.1.5` = "plyr_0.1.5.tar.gz", `0.1.6` = "plyr_0.1.6.tar.gz", 
                         `0.1.7` = "plyr_0.1.7.tar.gz", `0.1.8` = "plyr_0.1.8.tar.gz", 
                         `0.1.9` = "plyr_0.1.9.tar.gz", `0.1` = "plyr_0.1.tar.gz", 
                         `1.0.1` = "plyr_1.0.1.tar.gz", `1.0.2` = "plyr_1.0.2.tar.gz", 
                         `1.0.3` = "plyr_1.0.3.tar.gz", `1.0` = "plyr_1.0.tar.gz", 
                         `1.1` = "plyr_1.1.tar.gz", `1.2.1` = "plyr_1.2.1.tar.gz", 
                         `1.2` = "plyr_1.2.tar.gz", `1.4.1` = "plyr_1.4.1.tar.gz", 
                         `1.4` = "plyr_1.4.tar.gz", `1.5.1` = "plyr_1.5.1.tar.gz", 
                         `1.5.2` = "plyr_1.5.2.tar.gz", `1.5` = "plyr_1.5.tar.gz", 
                         `1.6` = "plyr_1.6.tar.gz", `1.7.1` = "plyr_1.7.1.tar.gz", 
                         `1.7` = "plyr_1.7.tar.gz", `1.8.1` = "plyr_1.8.1.tar.gz", 
                         `1.8` = "plyr_1.8.tar.gz"), 
                    .Names = c("0.1.1", "0.1.2", "0.1.3", "0.1.4", "0.1.5", "0.1.6", "0.1.7", "0.1.8", "0.1.9", "0.1", "1.0.1", "1.0.2", "1.0.3", "1.0", "1.1", "1.2.1", "1.2", "1.4.1", "1.4", "1.5.1", "1.5.2", "1.5", "1.6", "1.7.1", "1.7", "1.8.1", "1.8"))), .Names = c("baseurl", "ver")), 
  windows = structure(list(
    R3.2 = "http://cran.r-project.org/bin/windows/contrib/3.2/plyr_1.8.1.zip", 
    R3.1 = "http://cran.r-project.org/bin/windows/contrib/3.1/plyr_1.8.1.zip", 
    R3.0 = "http://cran.r-project.org/bin/windows/contrib/3.0/plyr_1.8.1.zip"), 
    .Names = c("R3.2", "R3.1", "R3.0")), 
  osx = structure(list(
    R3.1 = "http://cran.r-project.org/bin/macosx/contrib/3.1/plyr_1.8.1.tgz", 
    R3.0 = "http://cran.r-project.org/bin/macosx/contrib/3.0/plyr_1.8.1.tgz", 
    R3.1_mavericks = "http://cran.r-project.org/bin/macosx/mavericks/contrib/3.1/plyr_1.8.1.tgz"), 
    .Names = c("R3.1", "R3.0", "R3.1_mavericks"))), 
  .Names = c("package", "description", "snapshotId", "snapshotDate", "snapshotDiffId", "compatibitlityCheck", "message", "source", "windows", "osx"))



test_that("snapshot functions return correct results", {
  
  expect_is(mranSnapshots(), "character")
  expect_message(mranSnapshots(), "Dates and times are in GMT")
  
  
  snaps <- mranSnapshots("2014-06-25")
  expect_is(snaps, "character")
  expect_equal(length(snaps), 8L)
  
  setSnapshotInOptions(rrt_path, snapdate = "2014-08-01", autosnap = TRUE)
  expect_equal(getOption("RRT_snapshotID"), "2014-08-01_0500")
  
  snap <- getSnapshotId("2014-06-25", forceLast=TRUE)
  expect_equal(snap, "2014-06-25_2300")
  
  setSnapshotInOptions(rrt_path, snapdate = "2014-06-25_0500", autosnap = FALSE)
  
  expect_equal(getOption("RRT_snapshotID"), "2014-06-25_0500")
  
  x <- mranPkgMetadata(package="plyr", snapshot="2014-08-04")
  expect_equal(x, exp)
  
  expect_error(mranPkgMetadata(package="_not_exist_", snapshot="2014-08-04"), 
               "404 - Package not found, you don't have an internet connection, or other error.")
  
  
})


# cleanup
cleanRRTfolder()


