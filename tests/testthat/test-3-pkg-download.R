# tests for initialize
context("download from mran")



rrt_path <- "~/rrttemp"
source(system.file("tests/testthat/0-common-functions.R", package="RRT"))
cleanRRTfolder()
options(repos=c(CRAN="http://cran.revolutionanalytics.com/"))



test_that("downloadPackageFromMran downloads files using rsync", {
    
  createRepoFolders(rrt_path)
  
  instPath <- rrtPath(rrt_path, "src")
  expect_equal(dir(instPath), character(0))
  
  snapshotid <- getSnapshotId(snapshotdate = '2014-08-04')
  downloadPackageFromMran(repo=rrt_path, snapshotid=snapshotid, pkgs=c("plyr","ggplot2"), quiet=TRUE, 
                     downloadType="rsync")
  downloaded <- dir(instPath, recursive = TRUE)
  expect_equal(downloaded, c("ggplot2_1.0.0.tar.gz", "plyr_1.8.1.tar.gz"))
  
  cleanRRTfolder()
  
  
})

test_that("downloadPackageFromMran downloads files using default mechanism", {
  
  createRepoFolders(rrt_path)
  
  instPath <- rrtPath(rrt_path, "src")
  expect_equal(dir(instPath), character(0))
  
  snapshotid <- getSnapshotId(snapshotdate = '2014-08-04')
  downloadPackageFromMran(repo=rrt_path, snapshotid=snapshotid, pkgs=c("plyr","ggplot2"), quiet=TRUE, 
                          downloadType="default")
  downloaded <- dir(instPath, recursive = TRUE)
  expect_equal(downloaded, c("ggplot2_1.0.0.tar.gz", "plyr_1.8.1.tar.gz"))
  
  cleanRRTfolder()
  
  
})


test_that("pkgVersionAtSnapshot returns latest package version", {
  
  expect_equal(
    pkgVersionAtSnapshot(pkgs="plyr", "2014-08-01"),
    c(plyr = "plyr/plyr_1.8.1.tar.gz")
  )
    
  expect_equal(
    pkgVersionAtSnapshot(pkgs="ggplot2", "2014-08-01"),
    c(ggplot2 = "ggplot2/ggplot2_1.0.0.tar.gz")
  )
  
})
  

